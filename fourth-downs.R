library(purrr)
library(dplyr)
library(gt)
library(gtExtras)
library(cfb4th)
library(lubridate)
library(cbbplotR)
library(cbbdata)
library(magick)
library(furrr)
library(progressr)
library(webshot2)

save_crop_gt <- function(gt_obj, file, whitespace = 50) {
    gtExtras::gtsave_extra(gt_obj, paste0("./export/14_24_", file), zoom = 2)

    magick::image_read(paste0("./export/14_24_", file)) |>
        image_trim() |>
        image_border("white", glue::glue('{whitespace}x{whitespace}')) |>
        image_write(paste0("./export/14_24_", file))
}

# get all games ----
# games_raw = purrr::map(2014:2024, cfbfastR::cfbd_game_info)
# games = dplyr::bind_rows(games_raw) %>%
#     filter(home_division == "fbs" & away_division == "fbs")

# get all betting data for games ----
bets_raw = readRDS("./data/14_24_lines.RDS")
games = bets_raw %>% mutate(game_id = as.factor(game_id)) %>% distinct(game_id, .keep_all = T) %>% dplyr::select(game_id, start_date, home_team, home_conference, away_team, away_conference)
bets = bets_raw %>%
    dplyr::filter(home_classification == "fbs" & away_classification == "fbs") %>%
    dplyr::mutate(
        provider = factor(.data$provider,
                          c(
                              "consensus",
                              "ESPN Bet",
                              "DraftKings",
                              "teamrankings",
                              "numberfire",
                              "Caesars",
                              "Caesars (Pennsylvania)",
                              "William Hill (New Jersey)",
                              "SugarHouse",
                              "Bovada"
                          )),
        spread = as.numeric(.data$spread),
        over_under = as.numeric(.data$over_under)
    ) %>%
    dplyr::group_by(.data$game_id) %>%
    dplyr::arrange(.data$provider) %>%
    dplyr::slice(1) %>%
    dplyr::select(
        "game_id",
        "spread",
        "over_under"
    )
#
# retrieve all pbp and join with betting data ----
# pbp_raw = cfbfastR::load_cfb_pbp(2014:2024) %>%
#     dplyr::filter(game_id %in% bets$game_id) %>%
#     dplyr::left_join(bets, by = "game_id")
# pbp_raw %>% saveRDS("./data/14_24_pbp.RDS")
#
# add 4th down probs to pbp ----
# pbp_raw = readRDS("./data/14_24_pbp.RDS")
# pbp_chunks = purrr::map(sort(unique(pbp_raw$year)), function(x) {
#     pbp_raw %>%
#         dplyr::filter(
#             down == 4
#             # & game_id %in% games$game_id
#             & year == x
#         ) %>%
#         cfb4th::add_4th_probs()
# })
# pbp = dplyr::bind_rows(pbp_chunks)

# cache pbp with probs
# pbp %>% saveRDS("./data/14_24_pbp_4th.RDS")
#
# use make table function from bot to assemble full dataset ----
# pbp <- readRDS("./data/14_24_pbp_4th.RDS")
# tictoc::tic("Fourth down table probs assembly")
# fourth_table_probs_raw = lapply(1:nrow(pbp), function(i) {
#                 row = pbp[i, ]
#                 t = cfb4th::make_table_data(row)
#                 t$game_id = row$game_id
#                 t$play_id = row$id_play
#     #             p()
#                 return(t)
# })
# fourth_table_probs = dplyr::bind_rows(fourth_table_probs_raw)
# tictoc::toc()
# # cache decisions + probs
# fourth_table_probs %>% saveRDS("./data/14_24_4th_table_probs.RDS")

# # merge plays and probs to get final(-ish) dataset ----
pbp = readRDS("./data/14_24_pbp_4th.RDS")
slim_pbp = pbp %>%
    mutate(
        id_play = as.factor(id_play),
        game_id = as.factor(game_id),
        choice = dplyr::case_when(
            # football to punt
            play_type %in% c("Blocked Punt", "Punt","Safety", "Blocked Punt Touchdown","Punt Return Touchdown") | grepl("Punt", play_type) ~ "Punt",
            # field goal
            play_type %in% c("Field Goal Good", "Field Goal Missed","Blocked Field Goal") ~ "Field goal attempt",
            # go for it
            play_type %in% c("Pass Incompletion", "Pass Reception", "Passing Touchdown",
                             "Rush", "Rushing Touchdown", "Sack","Interception",
                             "Fumble Recovery (Opponent)","Pass Interception Return",
                             "Fumble Return Touchdown") | rush == 1 | pass == 1 ~ "Go for it",
            # penalty
            play_type %in% c("Penalty") ~ "Penalty",
            play_type %in% c("Timeout") ~ "Timeout",
            TRUE ~ NA_character_
        )
    ) %>%
    select(game_id, play_id = id_play, actual_choice = choice, play_type, play_text, pos_team, season = year, period, home_wp_before, away_wp_before)

fourth_table_probs = readRDS("./data/14_24_4th_table_probs.RDS") %>%
    mutate(
        play_id = as.factor(play_id),
        game_id = as.factor(game_id)
    )

fourth_down_decisions_raw = fourth_table_probs %>%
    dplyr::left_join(slim_pbp, by = c("game_id", "play_id")) %>%
    dplyr::group_by(game_id, play_id, choice) %>%
    dplyr::slice_min(order_by = dplyr::row_number(), n = 1, with_ties = F) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(games, by = "game_id") %>%
    dplyr::mutate(
        pos_team_conference = dplyr::case_when(
            pos_team == home_team ~ home_conference,
            pos_team == away_team ~ away_conference
        ),
        pos_team_wp_before = dplyr::case_when(
            pos_team == home_team ~ home_wp_before,
            pos_team == away_team ~ away_wp_before
        )
    )

# summarizer method
annotate_fourth_downs = function(df) {
    df %>%
        filter(
            !(actual_choice %in% c("Timeout", "Penalty"))
        ) %>%
        group_by(game_id, play_id) %>%
        arrange(-choice_prob) %>%
        mutate(
            optimal = row_number() == 1
        ) %>%
        summarize(
            season = last(season),
            pos_team = last(pos_team),
            pos_team_conference = last(pos_team_conference),
            period = last(period),
            pos_team_wp_before = last(pos_team_wp_before),
            actual_choice = last(actual_choice),
            optimal_choice = .data$choice[which(.data$optimal == 1)],
            wp_diff = .data$choice_prob[1] - .data$choice_prob[2]
        ) %>%
        ungroup() %>%
        filter(!is.na(wp_diff)) %>%
        mutate(
            confidence = case_when(
                abs(wp_diff) < 1 ~ "LOW",
                abs(wp_diff) >= 1 & abs(wp_diff) < 3 ~ "MEDIUM",
                abs(wp_diff) >= 3 & abs(wp_diff) <= 10 ~ "STRONG",
                abs(wp_diff) > 10 ~ "VERY STRONG"
            ),
            was_meaningful = (
                period == 1 | (pos_team_wp_before >= 0.2 & pos_team_wp_before <= 0.8)
            ),
            was_obvious = abs(wp_diff) >= 1.5 & was_meaningful,
            was_optimal = actual_choice == optimal_choice
        )
}

# get first pass data set with summarizer method
fourth_down_decisions = fourth_down_decisions_raw %>%
    # rows_upsert(missing_probs, by = c("game_id", "play_id", "choice")) %>%
    annotate_fourth_downs()

# handle rows with missing probs ----
# usually those with <= 30 sec on clock
missing_probs_raw = fourth_down_decisions %>%
    filter(is.na(wp_diff))

if (nrow(missing_probs_raw) > 0) {
    missing_probs_list = lapply(unique(missing_probs_raw$play_id), function(x) {
        base_play = pbp %>%
            filter(id_play == x) %>%
            group_by(game_id, id_play) %>%
            slice_min(order_by = row_number(), n = 1, with_ties = F) %>%
            ungroup()

        tryCatch({
            base_play %>%
                mutate(type = NA_character_) %>%
                select(-all_of(c("go_boost", "first_down_prob","wp_fail","wp_succeed","go_wp","fg_make_prob","miss_fg_wp","make_fg_wp","fg_wp","punt_wp"   ))) %>%
                cfb4th::add_4th_probs() %>%
                cfb4th::make_table_data() %>%
                mutate(
                    play_id = x,
                    game_id = base_play$game_id
                ) %>%
                return()
        }, error = function(e) {
            print(glue::glue("{x}: {e}"))
            return(NULL)
        })
    })

    missing_probs = missing_probs_list[!sapply(missing_probs_list, is.null)] %>%
        dplyr::bind_rows() %>%
        mutate(
            game_id = as.factor(game_id),
            play_id = as.factor(play_id)
        ) %>%
        left_join(slim_pbp, by = c("game_id", "play_id")) %>%
        group_by(game_id, play_id, choice) %>%
        slice_min(order_by = row_number(), n = 1, with_ties = F) %>%
        ungroup() %>%
        left_join(games %>% mutate(game_id = as.factor(game_id)) %>% select(game_id, home_team, home_conference, away_team, away_conference), by = "game_id") %>%
        mutate(
            pos_team_conference = dplyr::case_when(
                pos_team == home_team ~ home_conference,
                pos_team == away_team ~ away_conference
            ),
            pos_team_wp_before = dplyr::case_when(
                pos_team == home_team ~ home_wp_before,
                pos_team == away_team ~ away_wp_before
            )
        )

    # merge missing + existing datasets to get final-final dataset ----
    fourth_down_decisions <- fourth_down_decisions_raw %>%
        rows_upsert(missing_probs, by = c("game_id", "play_id", "choice")) %>%
        annotate_fourth_downs()
}


# cache final dataset
fourth_down_decisions %>% saveRDS("./data/fourth_down_decisions_14_24.RDS")

# Start actual analysis ----
fourth_down_decisions = readRDS("./data/fourth_down_decisions_14_24.RDS")

# Overall aggression on 4th downs / conversion attempts rate trend ----
fourth_down_decisions %>%
    group_by(season) %>%
    summarize(
        count = n(),
        n_convert = sum(actual_choice == "Go for it", na.rm = T),
        pct_convert = n_convert / count
    ) %>%
    ungroup() %>%
    gt() %>%
    gt_theme_538() %>%
    tab_header(
        title = md("**CFB Coaches are more aggressive on fourth down**"),
        subtitle = md("Since 2014, FBS head coaches have attempted fourth-down conversions more and more often.")
    ) %>%
    cols_align(columns = c("season"), align = "left") %>%
    cols_align(columns = c("count", "n_convert", "pct_convert"), align = "center") %>%
    fmt_percent(
        columns = pct_convert,
        decimals = 1
    ) %>%
    gt_color_rows(pct_convert, palette = "ggsci::green_material", direction = 1) |> # set green or red palette
    tab_style(
        style = list(
            cell_text(weight = "bold")
        ),
        locations = cells_body(
            columns = c("season"),
        )
    ) %>%
    tab_options(
        heading.align = "left"
    ) %>%
    cols_label(
        "season" ~ "Season",
        "count" ~  "4th Downs",
        "n_convert" ~ "Chose to Go",
        "pct_convert" ~ "Chose to Go %"
    ) %>%
    tab_source_note(source_note = md(
        "Data from cfbfastR and cfb4th. Table assembled by @akeaswaran."
    )) %>%
    tab_footnote(
        footnote = "COVID-shortened season.",
        locations = cells_body(columns = season, rows = 7)
    ) %>%
    save_crop_gt("4th_freq.png", 50)

# Obvious Go rate trends - Nationwide ----
analyze_decisions = function(df, actual_choice = NULL, optimal_choice = NULL, min_wp_diff = 0, min_count = 0, min_wp_before = 0, max_wp_before = 100, wp_or_quarter = NULL) {
    if (!is.null(actual_choice)) {
        df = df %>%
            filter(
                .data$actual_choice == !!(actual_choice)
            )
    } else if (!is.null(optimal_choice)) {
        df = df %>%
            filter(
                .data$optimal_choice == !!(optimal_choice)
            )
    }


    if (!is.null(wp_or_quarter)) {
        df = df %>%
            filter(
                wp_diff >= min_wp_diff
                & (period == wp_or_quarter | (pos_team_wp_before >= min_wp_before & pos_team_wp_before <= max_wp_before))
            )
    } else {
        df = df %>%
            filter(
                wp_diff >= min_wp_diff
                & pos_team_wp_before >= min_wp_before
                & pos_team_wp_before <= max_wp_before
            )
    }

    df %>%
        mutate(
            optimal_wp_diff = case_when(
                was_optimal ~ wp_diff,
                .default = NA_real_
            ),
            unoptimal_wp_diff = case_when(
                !was_optimal ~ -wp_diff,
                .default = NA_real_
            )
        ) %>%
        group_by(season, pos_team) %>%
        summarize(
            pos_team_conference = last(pos_team_conference),
            count = n(),
            n_optimal = sum(was_optimal, na.rm = T),
            optimal_wp_added = sum(optimal_wp_diff, na.rm = T),
            unoptimal_wp_added = sum(unoptimal_wp_diff, na.rm = T),
            n_unoptimal = count - n_optimal,
            pct_optimal = mean(was_optimal, na.rm = T),
            pct_unoptimal = n_unoptimal / count
        ) %>%
        ungroup() %>%
        filter(count >= min_count)
}

fourth_down_decisions %>%
    analyze_decisions(
        # actual_choice = "Go for it",
        optimal_choice = "Go for it",
        min_wp_diff = 1.5,
        min_wp_before = 0.1,
        max_wp_before = 1.0,
        wp_or_quarter = 1
    ) %>%
    group_by(season) %>%
    summarize(
        count = sum(count),
        n_optimal = sum(n_optimal),
        pct_optimal = n_optimal / count
    ) %>%
    ungroup() %>%
    gt() %>%
    gt_theme_538() %>%
    tab_header(
        title = md("**CFB Coaches are getting smarter on fourth downs**"),
        subtitle = md("Since 2014, FBS head coaches have made better decisions in 'obvious-go' situations.")
    ) %>%
    tab_footnote(
        footnote = "'obvious-go' situation: a fourth down where attempting the conversion has an expected win probability that is 1.5% or better than that of a field goal or punt AND is in the first quarter OR the offense has a win probability above 10%.",
        locations = cells_title(groups = "subtitle")
    ) %>%
    cols_align(columns = "season", align = "left") %>%
    cols_align(columns = c("count", "n_optimal", "pct_optimal"), align = "center") %>%
    fmt_percent(
        columns = pct_optimal,
        decimals = 1
    ) %>%
    gt_color_rows(pct_optimal, palette = "ggsci::green_material", direction = 1) |> # set green or red palette
    tab_style(
        style = list(
            cell_text(weight = "bold")
        ),
        locations = cells_body(
            columns = c("season"),
        )
    ) %>%
    tab_options(
        heading.align = "left"
    ) %>%
    cols_label(
        "season" ~ "Season",
        "count" ~  "Obvious-Go Situations",
        "n_optimal" ~ "Chose to Go",
        "pct_optimal" ~ "Chose to Go %"
    ) %>%
    tab_source_note(source_note = md(
        "Data from cfbfastR and cfb4th. Table assembled by @akeaswaran."
    )) %>%
    tab_footnote(
        footnote = "COVID-shortened season.",
        locations = cells_body(columns = season, rows = 7)
    )  %>%
    opt_css(
        '
      #table .gt_sourcenote {
        line-height: 1.2
      }
      #table .gt_footnote {
        line-height: 1.2
      }
      #table .gt_heading {
         padding-bottom: 0px;
         padding-top: 6px
      }
      #table .gt_subtitle {
         padding-top: 2px;
         padding-bottom: 6px;
      }
      '
    ) %>%
    save_crop_gt("obvious_go_nation.png", 50)

# Optimal Decisions WP added
optimal_wp_ranks = fourth_down_decisions %>%
    analyze_decisions(
        # actual_choice = "Go for it",
        optimal_choice = "Go for it",
        min_wp_diff = 1.5,
        min_wp_before = 0.1,
        max_wp_before = 1.0,
        wp_or_quarter = 1
    ) %>%
    mutate(
        net_wp_added = optimal_wp_added - abs(unoptimal_wp_added)
    ) %>%
    group_by(season) %>%
    arrange(-optimal_wp_added, -count) %>%
    mutate(
        rank_optimal_wp = row_number(),
        # total = n()
    ) %>%
    arrange(-unoptimal_wp_added, -count) %>%
    mutate(
        rank_unoptimal_wp = row_number(),
        # total = n()
    ) %>%
    arrange(-net_wp_added, -count) %>%
    mutate(
        rank_net_wp = row_number(),
        # total = n()
    ) %>%
    arrange(-pct_optimal, -count) %>%
    mutate(
        rank_optimal = row_number(),
        # total = n()
    ) %>%
    ungroup() %>%
    arrange(season) %>%
    filter(
        season == 2024
    ) %>%
    arrange(-optimal_wp_added) %>%
    select(-pos_team_conference, -season, -n_unoptimal, -pct_unoptimal)

optimal_wp_ranks %>%
    select(-unoptimal_wp_added, -rank_optimal_wp, -rank_unoptimal_wp, -rank_optimal, -rank_net_wp, -net_wp_added) %>%
    head(10) %>%
    cbbplotR::gt_cbb_teams(pos_team, pos_team, include_name = T) |>
    mutate(
        pos_team = glue::glue("#{row_number()} {pos_team}")
    ) %>%
    gt() %>%
    gt_theme_538() %>%
    tab_header(
        title = md(glue::glue("**What Happens in Vegas...**")),
        subtitle = md("UNLV rolled the dice and hit paydirt more often than not, adding nearly an entire win off optimal fourth-down decisions in 2024.")
    ) %>%
    tab_footnote(
        footnote = "'obvious-go' situation: a fourth down where attempting the conversion has an expected win probability that is 1.5% or better than that of a field goal or punt AND is in the first quarter OR the offense has a win probability above 10%.",
        locations = cells_column_labels(columns = "count")
    ) %>%
    cols_align(columns = c("pos_team"), align = "left") %>%
    cols_align(columns = c("count", "n_optimal"), align = "center") %>%
    cols_align(columns = c("pct_optimal", "optimal_wp_added"), align = "center") %>%
    fmt_percent(
        columns = pct_optimal,
        drop_trailing_zeros = T,
        decimals = 1
    ) %>%
    fmt_percent(
        columns = optimal_wp_added,
        decimals = 1,
        scale_values = F,
        force_sign = T,
        drop_trailing_zeros = T,
    ) %>%
    # fmt_number(
    #     columns = rank_optimal_wp,
    #     decimals = 0,
    #     pattern = "#{x}"
    # ) %>%
    fmt_markdown(pos_team) %>%
    # gt_color_rows(pct_optimal, palette = "ggsci::green_material", direction = 1, domain = 0:1) |> # show how percent can differ to relative performance
    gt_color_rows(optimal_wp_added, palette = "ggsci::green_material", direction = 1, domain = optimal_wp_ranks$optimal_wp_added) |>
    tab_style(
        style = list(
            cell_text(weight = "bold")
        ),
        locations = cells_body(
            columns = c("pos_team"),
        )
    ) %>%
    tab_options(
        heading.align = "left"
    ) %>%
    cols_label(
        # "season" ~ "Season",
        "pos_team" ~ "Team",
        "count" ~  "Obvious-Go Situations",
        "n_optimal" ~ "Chose to Go",
        "pct_optimal" ~ "Chose to Go %",
        "optimal_wp_added" ~ "Win Prob Gained",
        # "rank_optimal_wp" ~ "National Rank",
    ) %>%
    cols_move(optimal_wp_added, after = pct_optimal) %>%
    tab_source_note(source_note = md(
        "Data from cfbfastR and cfb4th. Logos from cbbdata. Table assembled by @akeaswaran."
    ))  %>%
    # gt_highlight_rows(
    #     rows = 1,
    #     fill = "#b3a369",
    #     # bold_target_only = TRUE,
    #     # target_col = car
    # ) %>%
    opt_css(
        '
      #table .gt_sourcenote {
      line-height: 1.2
      }
      #table .gt_heading {
         padding-bottom: 0px;
         padding-top: 6px
        }
      #table .gt_subtitle {
         padding-top: 2px;
         padding-bottom: 6px;
        }
      '
    ) %>%
    save_crop_gt("top_optimal_wp_added_nation.png")

optimal_wp_ranks %>%
    mutate(
        pct_unoptimal = 1 - pct_optimal
    ) %>%
    arrange(rank_unoptimal_wp) %>%
    tail(10) %>%
    select(-optimal_wp_added, -rank_optimal_wp, -rank_optimal, -rank_net_wp, -net_wp_added, -pct_optimal) %>%
    cbbplotR::gt_cbb_teams(pos_team, pos_team, include_name = T) |>
    mutate(
        pos_team = glue::glue("#{rank_unoptimal_wp} {pos_team}")
    ) %>%
    select(-rank_unoptimal_wp) %>%
    gt() %>%
    gt_theme_538() %>%
    tab_header(
        title = md(glue::glue("**Midwest Swoon**")),
        subtitle = md("Iowa State just barely edges out in-state rival Iowa in yet another battle for the CyHawk Trophy...except this title might not be one the Cyclones want to keep.")
    ) %>%
    tab_footnote(
        footnote = "'obvious-go' situation: a fourth down where attempting the conversion has an expected win probability that is 1.5% or better than that of a field goal or punt AND is in the first quarter OR the offense has a win probability above 10%.",
        locations = cells_column_labels(columns = count)
    ) %>%
    cols_align(columns = c("pos_team"), align = "left") %>%
    cols_align(columns = c("count", "n_optimal"), align = "center") %>%
    cols_align(columns = c("pct_unoptimal", "unoptimal_wp_added"), align = "center") %>%
    fmt_percent(
        columns = pct_unoptimal,
        drop_trailing_zeros = T,
        decimals = 1
    ) %>%
    fmt_percent(
        columns = unoptimal_wp_added,
        decimals = 1,
        drop_trailing_zeros = T,
        scale_values = F,
        force_sign = T
    ) %>%
    # fmt_number(
    #     columns = rank_optimal_wp,
    #     decimals = 0,
    #     pattern = "#{x}"
    # ) %>%
    fmt_markdown(pos_team) %>%
    gt_color_rows(unoptimal_wp_added, palette = "ggsci::green_material", direction = 1, domain = optimal_wp_ranks$unoptimal_wp_added) |>
    tab_style(
        style = list(
            cell_text(weight = "bold")
        ),
        locations = cells_body(
            columns = c("pos_team"),
        )
    ) %>%
    tab_options(
        heading.align = "left"
    ) %>%
    cols_label(
        # "season" ~ "Season",
        "pos_team" ~ "Team",
        "count" ~  "Obvious-Go Situations",
        "n_optimal" ~ "Chose to Go",
        "pct_unoptimal" ~ md("Chose to **Not** Go %"),
        "unoptimal_wp_added" ~ "Win Prob Lost",
        # "rank_optimal_wp" ~ "National Rank",
    ) %>%
    cols_move(unoptimal_wp_added, after = pct_unoptimal) %>%
    tab_source_note(source_note = md(
        "Data from cfbfastR and cfb4th. Logos from cbbdata. Table assembled by @akeaswaran."
    ))  %>%
    # gt_highlight_rows(
    #     rows = 6,
    #     fill = "#b3a369",
    #     # bold_target_only = TRUE,
    #     # target_col = car
    # ) %>%
    opt_css(
        '
      #table .gt_sourcenote {
      line-height: 1.2
      }
      #table .gt_heading {
         padding-bottom: 0px;
         padding-top: 6px
        }
      #table .gt_subtitle {
         padding-top: 2px;
         padding-bottom: 6px;
        }
      '
    ) %>%
    save_crop_gt("top_unoptimal_wp_added_nation.png")


optimal_wp_ranks %>%
    arrange(rank_net_wp) %>%
    tail(10) %>%
    select(-optimal_wp_added, -rank_optimal_wp, -rank_optimal, -rank_unoptimal_wp, -unoptimal_wp_added) %>%
    cbbplotR::gt_cbb_teams(pos_team, pos_team, include_name = T) |>
    mutate(
        pos_team = glue::glue("#{rank_net_wp} {pos_team}")
    ) %>%
    select(-rank_net_wp) %>%
    gt() %>%
    gt_theme_538() %>%
    tab_header(
        title = md(glue::glue("**Not So Smart?**")),
        subtitle = md("Kirby Smart continues to be wasteful on fourth down.")
    ) %>%
    tab_footnote(
        footnote = "'obvious-go' situation: a fourth down where attempting the conversion has an expected win probability that is 1.5% or better than that of a field goal or punt AND is in the first quarter OR the offense has a win probability above 10%.",
        locations = cells_column_labels(columns = count)
    ) %>%
    cols_align(columns = c("pos_team"), align = "left") %>%
    cols_align(columns = c("count", "n_optimal"), align = "center") %>%
    cols_align(columns = c("pct_optimal", "net_wp_added"), align = "center") %>%
    fmt_percent(
        columns = pct_optimal,
        drop_trailing_zeros = T,
        decimals = 1
    ) %>%
    fmt_percent(
        columns = net_wp_added,
        decimals = 1,
        drop_trailing_zeros = T,
        scale_values = F,
        force_sign = T
    ) %>%
    # fmt_number(
    #     columns = rank_optimal_wp,
    #     decimals = 0,
    #     pattern = "#{x}"
    # ) %>%
    fmt_markdown(pos_team) %>%
    gt_color_rows(net_wp_added, palette = "ggsci::green_material", direction = 1, domain = optimal_wp_ranks$net_wp_added) |>
    tab_style(
        style = list(
            cell_text(weight = "bold")
        ),
        locations = cells_body(
            columns = c("pos_team"),
        )
    ) %>%
    tab_options(
        heading.align = "left"
    ) %>%
    cols_label(
        # "season" ~ "Season",
        "pos_team" ~ "Team",
        "count" ~  "Obvious-Go Situations",
        "n_optimal" ~ "Chose to Go",
        "pct_optimal" ~ "Chose to Go %",
        "net_wp_added" ~ "Net Win Prob Gained",
        # "rank_optimal_wp" ~ "National Rank",
    ) %>%
    cols_move(net_wp_added, after = pct_optimal) %>%
    tab_source_note(source_note = md(
        "Data from cfbfastR and cfb4th. Logos from cbbdata. Table assembled by @akeaswaran."
    ))  %>%
    # gt_highlight_rows(
    #     rows = 6,
    #     fill = "#b3a369",
    #     # bold_target_only = TRUE,
    #     # target_col = car
    # ) %>%
    opt_css(
        '
      #table .gt_sourcenote {
      line-height: 1.2
      }
      #table .gt_heading {
         padding-bottom: 0px;
         padding-top: 6px
        }
      #table .gt_subtitle {
         padding-top: 2px;
         padding-bottom: 6px;
        }
      '
    ) %>%
    save_crop_gt("bottom_net_wp_added_nation.png")

optimal_wp_ranks %>%
    arrange(rank_net_wp) %>%
    head(10) %>%
    select(-optimal_wp_added, -rank_optimal_wp, -rank_optimal, -rank_unoptimal_wp, -unoptimal_wp_added) %>%
    cbbplotR::gt_cbb_teams(pos_team, pos_team, include_name = T) |>
    mutate(
        pos_team = glue::glue("#{rank_net_wp} {pos_team}")
    ) %>%
    select(-rank_net_wp) %>%
    gt() %>%
    gt_theme_538() %>%
    tab_header(
        title = md(glue::glue("**What Happens in Vegas...**")),
        subtitle = md("UNLV managed to limit the damage of even their bad go decisions and still net over half a win on their fourth-down decision making in 2024.")
    ) %>%
    tab_footnote(
        footnote = "'obvious-go' situation: a fourth down where attempting the conversion has an expected win probability that is 1.5% or better than that of a field goal or punt AND is in the first quarter OR the offense has a win probability above 10%.",
        locations = cells_column_labels(columns = count)
    ) %>%
    cols_align(columns = c("pos_team"), align = "left") %>%
    cols_align(columns = c("count", "n_optimal"), align = "center") %>%
    cols_align(columns = c("pct_optimal", "net_wp_added"), align = "center") %>%
    fmt_percent(
        columns = pct_optimal,
        drop_trailing_zeros = T,
        decimals = 1
    ) %>%
    fmt_percent(
        columns = net_wp_added,
        decimals = 1,
        drop_trailing_zeros = T,
        scale_values = F,
        force_sign = T
    ) %>%
    # fmt_number(
    #     columns = rank_optimal_wp,
    #     decimals = 0,
    #     pattern = "#{x}"
    # ) %>%
    fmt_markdown(pos_team) %>%
    gt_color_rows(net_wp_added, palette = "ggsci::green_material", direction = 1, domain = optimal_wp_ranks$net_wp_added) |>
    tab_style(
        style = list(
            cell_text(weight = "bold")
        ),
        locations = cells_body(
            columns = c("pos_team"),
        )
    ) %>%
    tab_options(
        heading.align = "left"
    ) %>%
    cols_label(
        # "season" ~ "Season",
        "pos_team" ~ "Team",
        "count" ~  "Obvious-Go Situations",
        "n_optimal" ~ "Chose to Go",
        "pct_optimal" ~ "Chose to Go %",
        "net_wp_added" ~ "Net Win Prob Gained",
        # "rank_optimal_wp" ~ "National Rank",
    ) %>%
    cols_move(net_wp_added, after = pct_optimal) %>%
    tab_source_note(source_note = md(
        "Data from cfbfastR and cfb4th. Logos from cbbdata. Table assembled by @akeaswaran."
    ))  %>%
    # gt_highlight_rows(
    #     rows = 6,
    #     fill = "#b3a369",
    #     # bold_target_only = TRUE,
    #     # target_col = car
    # ) %>%
    opt_css(
        '
      #table .gt_sourcenote {
      line-height: 1.2
      }
      #table .gt_heading {
         padding-bottom: 0px;
         padding-top: 6px
        }
      #table .gt_subtitle {
         padding-top: 2px;
         padding-bottom: 6px;
        }
      '
    ) %>%
    save_crop_gt("top_net_wp_added_nation.png")

# metric stability ----
team_season_performance = fourth_down_decisions %>%
    analyze_decisions(
        # actual_choice = "Go for it",
        optimal_choice = "Go for it",
        min_wp_diff = 1.5,
        min_wp_before = 0.1,
        max_wp_before = 1,
        wp_or_quarter = 1
    )

team_season_performance %>%
    group_by(pos_team) %>%
    arrange(season) %>%
    mutate(
        lag_pct_optimal = lag(pct_optimal),
        lag_n_optimal = lag(n_optimal),
        lag_count = lag(count)
    ) %>%
    ungroup() %>%
    filter(!is.na(lag_pct_optimal)) %>%
    # group_by(season) %>%
    summarize(
        pct_optimal_yoy = cor(lag_pct_optimal, pct_optimal, use = "complete.obs"),
        n_optimal_yoy = cor(lag_n_optimal, n_optimal, use = "complete.obs"),
        count_yoy = cor(lag_count, count, use = "complete.obs")
    )

# overcorrection ----
fourth_down_decisions %>%
    filter(
        optimal_choice != "Go for it"
        & was_obvious
    ) %>%
    # analyze_decisions(
    #     # actual_choice = "Go for it",
    #     # optimal_choice = "Go for it",
    #     # min_wp_diff = 1.5
    # ) %>%
    group_by(season) %>% #, pos_team) %>%
    summarize(
        count = n(),
        n_go = sum(actual_choice == "Go for it", na.rm = T),
        pct_go = n_go / count
        # n_optimal = sum(n_optimal),
        # n_unoptimal = sum(optimal_choice != "Go for it"),
        # pct_unoptimal = n_unoptimal / count
    ) %>%
    ungroup() %>%
    gt() %>%
    gt_theme_538() %>%
    tab_header(
        title = md("**CFB Coaches aren't overcorrecting**"),
        subtitle = md("Contrary to what _some_ coaches might believe, FBS head coaches generally haven't started attempting fourth-down conversions when they obviously should not.")
    ) %>%
    tab_footnote(
        footnote = "'obvious' situation: a fourth down where the optimal choice has an expected win probability that is 1.5% or better than that of the other choices.",
        locations = cells_title(groups = "subtitle")
    ) %>%
    cols_align(columns = "season", align = "left") %>%
    cols_align(columns = c("count", "n_go", "pct_go"), align = "center") %>%
    fmt_percent(
        columns = pct_go,
        decimals = 1,
        drop_trailing_zeros = T
    ) %>%
    gt_color_rows(pct_go, palette = "ggsci::green_material", direction = 1, domain = 0:1) |>
    tab_style(
        style = list(
            cell_text(weight = "bold")
        ),
        locations = cells_body(
            columns = c("season"),
        )
    ) %>%
    tab_options(
        heading.align = "left"
    ) %>%
    cols_label(
        "season" ~ "Season",
        "count" ~  "Obvious Non-Go Situations",
        "n_go" ~ "Chose to Go",
        "pct_go" ~ "Chose to Go %"
    ) %>%
    tab_source_note(source_note = md(
        "Data from cfbfastR and cfb4th. Table assembled by @akeaswaran."
    )) %>%
    tab_footnote(
        footnote = "COVID-shortened season.",
        locations = cells_body(columns = season, rows = 7)
    ) %>%
    save_crop_gt("overcorrection.png", 50)

# teams that overcorrected the most
team_overcorrection = fourth_down_decisions %>%
    filter(
        optimal_choice != "Go for it"
        & wp_diff >= 1.5
        & was_obvious
    ) %>%
    # analyze_decisions(
    #     # actual_choice = "Go for it",
    #     # optimal_choice = "Go for it",
    #     # min_wp_diff = 1.5
    # ) %>%
    group_by(season, pos_team) %>%
    summarize(
        count = n(),
        n_go = sum(actual_choice == "Go for it", na.rm = T),
        pct_go = n_go / count
        # n_optimal = sum(n_optimal),
        # n_unoptimal = sum(optimal_choice != "Go for it"),
        # pct_unoptimal = n_unoptimal / count
    ) %>%
    ungroup() %>%
    arrange(-pct_go) %>%
    filter(count >= 5)

team_overcorrection %>%
    head(10) %>%
    cbbplotR::gt_cbb_teams(pos_team, pos_team, include_name = T) %>%
    gt() %>%
    gt_theme_538() %>%
    fmt_markdown(pos_team) %>%
    tab_header(
        title = md("**If overcorrection _does_ happen, it's in extremely small samples**"),
        subtitle = md('Below are the top 10 \'overcorrecting\' teams by go rate in \'obvious non-go\' situations since 2014. Notice that the raw number of times that these teams chose to go is extremely small.')
    ) %>%
    tab_footnote(
        footnote = "'obvious' situation: a fourth down where the optimal choice has an expected win probability that is 1.5% or better than that of the other choices.",
        locations = cells_title(groups = "subtitle")
    ) %>%
    cols_align(columns = c("season", "pos_team"), align = "left") %>%
    cols_align(columns = c("count", "n_go", "pct_go"), align = "center") %>%
    fmt_percent(
        columns = pct_go,
        decimals = 1,
        drop_trailing_zeros = T
    ) %>%
    gt_color_rows(pct_go, palette = "ggsci::green_material", direction = 1, domain = team_overcorrection$pct_go) |>
    tab_style(
        style = list(
            cell_text(weight = "bold")
        ),
        locations = cells_body(
            columns = c("season", "pos_team"),
        )
    ) %>%
    tab_options(
        heading.align = "left"
    ) %>%
    cols_label(
        "season" ~ "Season",
        "pos_team" ~ "Team",
        "count" ~  "Obvious Non-Go Situations",
        "n_go" ~ "Chose to Go",
        "pct_go" ~ "Chose to Go %"
    ) %>%
    tab_source_note(source_note = md(
        "Data from cfbfastR and cfb4th. Table assembled by @akeaswaran. Note: table includes team-seasons with 5+ 'obvious non-go' situations."
    )) %>%
    save_crop_gt("overcorrection_team_seasons.png") #%>%
    # tab_footnote(
    #     footnote = "COVID-shortened season.",
    #     locations = cells_body(columns = season, rows = 7)
    # )

# Coach/team specific analysis ----
generate_coach_table_graphic <- function(df, title, subtitle, team = "Georgia Tech", min_sample_size = 0) {
    tb = df %>%
        group_by(season) %>%
        arrange(-pct_optimal) %>%
        mutate(
            rank_optimal = row_number()
        ) %>%
        ungroup() %>%
        filter(pos_team == !!team) %>%
        arrange(season) %>%
        select(-n_unoptimal, -pct_unoptimal, -pos_team_conference, -optimal_wp_added, -unoptimal_wp_added) %>%
        cbbplotR::gt_cbb_teams(pos_team, pos_team, include_name = T) |>
        # mutate(pos_team = 'https://a.espncdn.com/i/teamlogos/ncaa/500/59.png') %>%
        gt(id = "table") %>%
        gt_theme_538() %>%
        tab_header(
            title = md(glue::glue("**{title}**")),
            subtitle = md(subtitle)
        ) %>%
        tab_footnote(
            footnote = "'obvious-go' situation: a fourth down where attempting the conversion has an expected win probability that is 1.5% or better than that of a field goal or punt AND is in the first quarter OR the offense has a win probability above 10%.",
            locations = cells_title(groups = "subtitle")
        ) %>%
        cols_align(columns = c("season", "pos_team"), align = "left") %>%
        cols_align(columns = c("count", "n_optimal"), align = "center") %>%
        cols_align(columns = c("pct_optimal", "rank_optimal"), align = "center") %>%
        fmt_percent(
            columns = pct_optimal,
            drop_trailing_zeros = T,
            decimals = 1
        ) %>%
        fmt_number(
            columns = rank_optimal,
            decimals = 0,
            pattern = "#{x}"
        ) %>%
        fmt_markdown(pos_team) %>%
        gt_color_rows(pct_optimal, palette = "ggsci::green_material", direction = 1, domain = 0:1) |> # show how percent can differ to relative performance
        gt_color_rows(rank_optimal, palette = "ggsci::blue_material", direction = -1, domain = 1:133) |>
        tab_style(
            style = list(
                cell_text(weight = "bold")
            ),
            locations = cells_body(
                columns = c("season", "pos_team"),
            )
        ) %>%
        tab_options(
            heading.align = "left"
        ) %>%
        cols_label(
            "season" ~ "Season",
            "pos_team" ~ "Team",
            "count" ~  "Obvious-Go Situations",
            "n_optimal" ~ "Chose to Go",
            "pct_optimal" ~ "Chose to Go %",
            "rank_optimal" ~ "National Rank"
        ) %>%
        tab_source_note(source_note = md(
            "Data from cfbfastR and cfb4th. Logos from cbbdata. Table assembled by @akeaswaran."
        )) %>%
        opt_css(
            '
      #table .gt_sourcenote {
      line-height: 1.2
      }
      #table .gt_heading {
         padding-bottom: 0px;
         padding-top: 6px
        }
      #table .gt_subtitle {
         padding-top: 2px;
         padding-bottom: 6px;
        }
      '
        )

    if (min_sample_size > 0) {
        tb = tb %>%
            tab_footnote(
                footnote = glue::glue("Rank based on teams in given season with {min_sample_size}+ 'obvious-go' situations."),
                locations = cells_column_labels(columns = "rank_optimal")
            )
    }
    return(tb)
}

generate_team_table_graphic <- function(team, subtitle, min_sample_size = 0) {
    fourth_down_decisions %>%
        analyze_decisions(
            optimal_choice = "Go for it",
            min_wp_diff = 1.5,
            min_count = min_sample_size,
            min_wp_before = 0.1,
            max_wp_before = 1.0,
            wp_or_quarter = 1
        ) %>%
        generate_coach_table_graphic(
            glue::glue("How did {team} perform on fourth downs?"),
            subtitle,
            team,
            min_sample_size = min_sample_size
        )
}

# Determine GT coach performance ----
cpj_era = games %>%
    filter(season %in% 2008:2018)

cpj = fourth_down_decisions %>%
    filter(game_id %in% cpj_era$game_id) %>%
    analyze_decisions()

fourth_down_decisions %>%
    filter(game_id %in% cpj_era$game_id) %>%
    analyze_decisions(
        optimal_choice = "Go for it",
        min_wp_diff = 1.5,
        # min_count = 3,
        min_wp_before = 0.1,
        max_wp_before = 1.0,
        wp_or_quarter = 1
    ) %>%
    generate_coach_table_graphic(
        "Paul Johnson knew what he was doing",
        "Whether by data or gut feeling, Johnson consistently outperformed his peers in 'obvious-go' situations.",
        # min_sample_size = 5
    ) %>%
    save_crop_gt("cpj.png")

geoff_era = games %>%
    filter(season >= 2019 & start_date <= lubridate::ymd('2022-09-25'))

fourth_down_decisions %>%
    filter(game_id %in% geoff_era$game_id) %>%
    analyze_decisions(
        optimal_choice = "Go for it",
        min_wp_diff = 1.5,
        min_wp_before = 0.1,
        max_wp_before = 1.0,
        wp_or_quarter = 1
        # min_count = 5
    ) %>%
    generate_coach_table_graphic(
        "Geoff Collins defies our priors",
        "Improbably, Collins had a number of opportunities to win on the margins in 'obvious-go' situations and took many of them."
    ) %>%
    tab_footnote(
        footnote = "COVID-shortened season.",
        locations = cells_body(columns = season, rows = 2)
    ) %>%
    tab_footnote(
        footnote = "Fired on September 25, 2022.",
        locations = cells_body(columns = season, rows = 4)
    ) %>%
    save_crop_gt("geoff.png")

bront_era = games %>%
    filter(start_date > lubridate::ymd('2022-09-25'))

fourth_down_decisions %>%
    filter(game_id %in% bront_era$game_id) %>%
    analyze_decisions(
        optimal_choice = "Go for it",
        min_wp_diff = 1.5,
        min_wp_before = 0.1,
        max_wp_before = 1.0,
        wp_or_quarter = 1
        # min_count = 5
    ) %>%
    generate_coach_table_graphic(
        "Brent Key is as conservative as you think he is",
        "Key's only had a handful of opportunities, but he should get better at this over time...right?."
    ) %>%
    tab_footnote(
        footnote = "Promoted to interim on September 25, 2022.",
        locations = cells_body(columns = season, rows = 1)
    ) %>%
    save_crop_gt("bront.png")


# Conference historic performance ----

unique_confs = length(unique(fourth_down_decisions$pos_team_conference))

generate_conference_table_graphic = function(df, conference, title, subtitle) {
    df %>%
        group_by(season, pos_team_conference) %>%
        summarize(
            count = sum(count),
            n_optimal = sum(n_optimal),
            pct_optimal = n_optimal / count
        ) %>%
        ungroup() %>%
        group_by(season) %>%
        arrange(-pct_optimal) %>%
        mutate(
            rank_optimal = row_number(),
            total = n()
        ) %>%
        ungroup() %>%
        arrange(season) %>%
        filter(pos_team_conference == !!conference) %>%
        cbbplotR::gt_cbb_conferences(pos_team_conference, pos_team_conference, include_name = T) |>
        gt() %>%
        gt_theme_538() %>%
        tab_header(
            title = md(glue::glue("**{title}**")),
            subtitle = md(subtitle)
        ) %>%
        tab_footnote(
            footnote = "'obvious-go' situation: a fourth down where attempting the conversion has an expected win probability that is 1.5% or better than that of a field goal or punt AND is in the first quarter OR the offense has a win probability above 10%.",
            locations = cells_title(groups = "subtitle")
        ) %>%
        cols_align(columns = c("season", "pos_team_conference"), align = "left") %>%
        cols_align(columns = c("count", "n_optimal"), align = "center") %>%
        cols_align(columns = c("pct_optimal", "rank_optimal"), align = "center") %>%
        fmt_percent(
            columns = pct_optimal,
            drop_trailing_zeros = T,
            decimals = 1
        ) %>%
        fmt_number(
            columns = rank_optimal,
            decimals = 0,
            pattern = "#{x}"
        ) %>%
        fmt_markdown(pos_team_conference) %>%
        gt_color_rows(pct_optimal, palette = "ggsci::green_material", direction = 1, domain = 0:1) |> # show how percent can differ to relative performance
        gt_color_rows(rank_optimal, palette = "ggsci::blue_material", direction = -1, domain = 1:unique_confs) |>
        tab_style(
            style = list(
                cell_text(weight = "bold")
            ),
            locations = cells_body(
                columns = c("season", "pos_team_conference"),
            )
        ) %>%
        tab_options(
            heading.align = "left"
        ) %>%
        cols_label(
            "season" ~ "Season",
            "pos_team_conference" ~ "Conference",
            "count" ~  "Obvious-Go Situations",
            "n_optimal" ~ "Chose to Go",
            "pct_optimal" ~ "Chose to Go %",
            "rank_optimal" ~ "Conf Rank",
        ) %>%
        cols_hide(
            columns = total
        ) %>%
        tab_source_note(source_note = md(
            "Data from cfbfastR and cfb4th. Logos from cbbdata. Table assembled by @akeaswaran."
        ))  %>%
        tab_footnote(
            footnote = "COVID-shortened season.",
            locations = cells_body(columns = season, rows = 7)
        ) %>%
        opt_css(
            '
      #table .gt_sourcenote {
      line-height: 1.2
      }
      #table .gt_heading {
         padding-bottom: 0px;
         padding-top: 6px
        }
      #table .gt_subtitle {
         padding-top: 2px;
         padding-bottom: 6px;
        }
      '
        )
}

fourth_down_decisions %>%
    analyze_decisions(
        optimal_choice = "Go for it",
        min_wp_diff = 1.5,
        min_wp_before = 0.1,
        max_wp_before = 1.0,
        wp_or_quarter = 1
        # min_count = 3
    ) %>%
    generate_conference_table_graphic("ACC", "The ACC is, as always, unpredictable", "A conference that now improbably includes schools from California and Texas ramped up how it dealt with 'obvious-go' situations in 2024.") %>%
    save_crop_gt("acc_trends.png")

generate_conference_team_graphic = function(df, conference, year, title, subtitle) {
    df %>%
        # group_by(season, pos_team_conference) %>%
        # summarize(
        #     count = sum(count),
        #     n_optimal = sum(n_optimal),
        #     pct_optimal = n_optimal / count
        # ) %>%
        # ungroup() %>%
        group_by(season) %>%
        arrange(-pct_optimal, -count) %>%
        mutate(
            rank_optimal = row_number(),
            # total = n()
        ) %>%
        ungroup() %>%
        arrange(season) %>%
        filter(
            pos_team_conference == !!conference
            & season == !!year
        ) %>%
        arrange(-pct_optimal) %>%
        select(-pos_team_conference, -season, -n_unoptimal, -pct_unoptimal, -optimal_wp_added, -unoptimal_wp_added) %>%
        cbbplotR::gt_cbb_teams(pos_team, pos_team, include_name = T) |>
        mutate(
            pos_team = glue::glue("#{row_number()} {pos_team}")
        ) %>%
        gt() %>%
        gt_theme_538() %>%
        tab_header(
            title = md(glue::glue("**{title}**")),
            subtitle = md(subtitle)
        ) %>%
        tab_footnote(
            footnote = "'obvious-go' situation: a fourth down where attempting the conversion has an expected win probability that is 1.5% or better than that of a field goal or punt AND is in the first quarter OR the offense has a win probability above 10%.",
            locations = cells_title(groups = "subtitle")
        ) %>%
        cols_align(columns = c("pos_team"), align = "left") %>%
        cols_align(columns = c("count", "n_optimal"), align = "center") %>%
        cols_align(columns = c("pct_optimal", "rank_optimal"), align = "center") %>%
        fmt_percent(
            columns = pct_optimal,
            drop_trailing_zeros = T,
            decimals = 1
        ) %>%
        fmt_number(
            columns = rank_optimal,
            decimals = 0,
            pattern = "#{x}"
        ) %>%
        fmt_markdown(pos_team) %>%
        gt_color_rows(pct_optimal, palette = "ggsci::green_material", direction = 1, domain = 0:1) |> # show how percent can differ to relative performance
        gt_color_rows(rank_optimal, palette = "ggsci::blue_material", direction = -1, domain = 1:133) |>
        tab_style(
            style = list(
                cell_text(weight = "bold")
            ),
            locations = cells_body(
                columns = c("pos_team"),
            )
        ) %>%
        tab_options(
            heading.align = "left"
        ) %>%
        cols_label(
            # "season" ~ "Season",
            "pos_team" ~ "Team",
            "count" ~  "Obvious-Go Situations",
            "n_optimal" ~ "Chose to Go",
            "pct_optimal" ~ "Chose to Go %",
            "rank_optimal" ~ "National Rank",
        ) %>%
        # cols_hide(
        #     columns = total
        # ) %>%
        tab_source_note(source_note = md(
            "Data from cfbfastR and cfb4th. Logos from cbbdata. Table assembled by @akeaswaran."
        ))  %>%
        # tab_footnote(
        #     footnote = "COVID-shortened season.",
        #     locations = cells_body(columns = season, rows = 7)
        # ) %>%
        opt_css(
            '
      #table .gt_sourcenote {
      line-height: 1.2
      }
      #table .gt_heading {
         padding-bottom: 0px;
         padding-top: 6px
        }
      #table .gt_subtitle {
         padding-top: 2px;
         padding-bottom: 6px;
        }
      '
        )
}

generate_conference_optimal_wp_added_graphic = function(df, conference, year, title, subtitle) {
    df %>%
        group_by(season) %>%
        arrange(-optimal_wp_added, -count) %>%
        mutate(
            rank_optimal_wp = row_number(),
            # total = n()
        ) %>%
        arrange(-pct_optimal, -count) %>%
        mutate(
            rank_optimal = row_number(),
            # total = n()
        ) %>%
        ungroup() %>%
        arrange(season) %>%
        filter(
            pos_team_conference == !!conference
            & season == !!year
        ) %>%
        arrange(-optimal_wp_added) %>%
        select(-pos_team_conference, -season, -n_unoptimal, -pct_unoptimal, -rank_optimal,-unoptimal_wp_added) %>%
        cbbplotR::gt_cbb_teams(pos_team, pos_team, include_name = T) |>
        mutate(
            pos_team = glue::glue("#{row_number()} {pos_team}")
        ) %>%
        gt() %>%
        gt_theme_538() %>%
        tab_header(
            title = md(glue::glue("**{title}**")),
            subtitle = md(subtitle)
        ) %>%
        tab_footnote(
            footnote = "'obvious-go' situation: a fourth down where attempting the conversion has an expected win probability that is 1.5% or better than that of a field goal or punt AND is in the first quarter OR the offense has a win probability above 10%.",
            locations = cells_title(groups = "subtitle")
        ) %>%
        cols_align(columns = c("pos_team"), align = "left") %>%
        cols_align(columns = c("count", "n_optimal"), align = "center") %>%
        cols_align(columns = c("pct_optimal", "rank_optimal_wp", "optimal_wp_added"), align = "center") %>%
        fmt_percent(
            columns = pct_optimal,
            drop_trailing_zeros = T,
            decimals = 1
        ) %>%
        fmt_percent(
            columns = optimal_wp_added,
            decimals = 1,
            scale_values = F,
            force_sign = T
        ) %>%
        fmt_number(
            columns = rank_optimal_wp,
            decimals = 0,
            pattern = "#{x}"
        ) %>%
        fmt_markdown(pos_team) %>%
        gt_color_rows(optimal_wp_added, palette = "ggsci::green_material", direction = 1, domain = optimal_wp_ranks$optimal_wp_added) |> # show how percent can differ to relative performance
        gt_color_rows(rank_optimal_wp, palette = "ggsci::blue_material", direction = -1, domain = 1:133) |>
        tab_style(
            style = list(
                cell_text(weight = "bold")
            ),
            locations = cells_body(
                columns = c("pos_team"),
            )
        ) %>%
        tab_options(
            heading.align = "left"
        ) %>%
        cols_label(
            # "season" ~ "Season",
            "pos_team" ~ "Team",
            "count" ~  "Obvious-Go Situations",
            "n_optimal" ~ "Chose to Go",
            "pct_optimal" ~ "Chose to Go %",
            "optimal_wp_added" ~ "Win Prob Gained",
            "rank_optimal_wp" ~ "National Rank",
        ) %>%
        cols_move(optimal_wp_added, after = pct_optimal) %>%
        tab_source_note(source_note = md(
            "Data from cfbfastR and cfb4th. Logos from cbbdata. Table assembled by @akeaswaran."
        ))  %>%
        # tab_footnote(
        #     footnote = "COVID-shortened season.",
        #     locations = cells_body(columns = season, rows = 7)
        # ) %>%
        opt_css(
            '
      #table .gt_sourcenote {
      line-height: 1.2
      }
      #table .gt_heading {
         padding-bottom: 0px;
         padding-top: 6px
        }
      #table .gt_subtitle {
         padding-top: 2px;
         padding-bottom: 6px;
        }
      '
        )
}

generate_conference_unoptimal_wp_added_graphic = function(df, conference, year, title, subtitle) {
    df %>%
        group_by(season) %>%
        arrange(-unoptimal_wp_added, -count) %>%
        mutate(
            rank_unoptimal_wp = row_number(),
            # total = n()
        ) %>%
        arrange(-pct_optimal, -count) %>%
        mutate(
            rank_optimal = row_number(),
            # total = n()
        ) %>%
        ungroup() %>%
        arrange(season) %>%
        filter(
            pos_team_conference == !!conference
            & season == !!year
        ) %>%
        arrange(-unoptimal_wp_added) %>%
        select(-pos_team_conference, -season, -n_unoptimal, -pct_optimal, -rank_optimal, -optimal_wp_added) %>%
        cbbplotR::gt_cbb_teams(pos_team, pos_team, include_name = T) |>
        mutate(
            pos_team = glue::glue("#{row_number()} {pos_team}")
        ) %>%
        gt() %>%
        gt_theme_538() %>%
        tab_header(
            title = md(glue::glue("**{title}**")),
            subtitle = md(subtitle)
        ) %>%
        tab_footnote(
            footnote = "'obvious-go' situation: a fourth down where attempting the conversion has an expected win probability that is 1.5% or better than that of a field goal or punt AND is in the first quarter OR the offense has a win probability above 10%.",
            locations = cells_title(groups = "subtitle")
        ) %>%
        cols_align(columns = c("pos_team"), align = "left") %>%
        cols_align(columns = c("count", "n_optimal"), align = "center") %>%
        cols_align(columns = c("pct_unoptimal", "rank_unoptimal_wp", "unoptimal_wp_added"), align = "center") %>%
        fmt_percent(
            columns = pct_unoptimal,
            drop_trailing_zeros = T,
            decimals = 1
        ) %>%
        fmt_percent(
            columns = unoptimal_wp_added,
            decimals = 1,
            scale_values = F,
            force_sign = T
        ) %>%
        fmt_number(
            columns = rank_unoptimal_wp,
            decimals = 0,
            pattern = "#{x}"
        ) %>%
        fmt_markdown(pos_team) %>%
        gt_color_rows(unoptimal_wp_added, palette = "ggsci::green_material", direction = 1, domain = optimal_wp_ranks$unoptimal_wp_added) |> # show how percent can differ to relative performance
        gt_color_rows(rank_unoptimal_wp, palette = "ggsci::blue_material", direction = -1, domain = 1:133) |>
        tab_style(
            style = list(
                cell_text(weight = "bold")
            ),
            locations = cells_body(
                columns = c("pos_team"),
            )
        ) %>%
        tab_options(
            heading.align = "left"
        ) %>%
        cols_label(
            # "season" ~ "Season",
            "pos_team" ~ "Team",
            "count" ~  "Obvious-Go Situations",
            "n_optimal" ~ "Chose to Go",
            "pct_unoptimal" ~ md("Chose to **Not** Go %"),
            "unoptimal_wp_added" ~ "Win Prob Lost",
            "rank_unoptimal_wp" ~ "National Rank",
        ) %>%
        cols_move(unoptimal_wp_added, after = pct_unoptimal) %>%
        tab_source_note(source_note = md(
            "Data from cfbfastR and cfb4th. Logos from cbbdata. Table assembled by @akeaswaran."
        ))  %>%
        # tab_footnote(
        #     footnote = "COVID-shortened season.",
        #     locations = cells_body(columns = season, rows = 7)
        # ) %>%
        opt_css(
            '
      #table .gt_sourcenote {
      line-height: 1.2
      }
      #table .gt_heading {
         padding-bottom: 0px;
         padding-top: 6px
        }
      #table .gt_subtitle {
         padding-top: 2px;
         padding-bottom: 6px;
        }
      '
        )
}

fourth_down_decisions %>%
    analyze_decisions(
        optimal_choice = "Go for it",
        min_wp_diff = 1.5,
        min_wp_before = 0.1,
        max_wp_before = 1.0,
        wp_or_quarter = 1
        # min_count = 5
    ) %>%
    generate_conference_team_graphic("ACC", 2024, "Boston College: The Smartest School in the ACC?", "In a surprise twist, the Eagles -- not conference champion FSU -- led the ACC in 'obvious-go' rate in 2024.") %>%
    save_crop_gt("acc_rate.png")


fourth_down_decisions %>%
    analyze_decisions(
        optimal_choice = "Go for it",
        min_wp_diff = 1.5,
        min_wp_before = 0.1,
        max_wp_before = 1.0,
        wp_or_quarter = 1,
        # min_count = 5
    ) %>%
    generate_conference_optimal_wp_added_graphic("ACC", 2024, "Demon Deacons pass the test", "While BC led the conference in 'obvious-go' rate in 2024, Wake Forest gained the most win probability off of their go decisions.") %>%
    save_crop_gt("optimal_wp_added_acc.png")

fourth_down_decisions %>%
    analyze_decisions(
        optimal_choice = "Go for it",
        min_wp_diff = 1.5,
        min_wp_before = 0.1,
        max_wp_before = 1.0,
        wp_or_quarter = 1,
        # min_count = 5
    ) %>%
    generate_conference_unoptimal_wp_added_graphic("ACC", 2024, "Tropical Depression", "Despite being runner up the ACC in win probability gained off correct 'obvious-go' decisions (+32.6%), Miami nearly gave that entire advantage back with their missed decisions.") %>%
    save_crop_gt("unoptimal_wp_added_acc.png")

generate_conference_net_wp_added_graphic = function(df, conference, year, title, subtitle) {
    df %>%
        mutate(
            net_wp_added = optimal_wp_added - abs(unoptimal_wp_added)
        ) %>%
        group_by(season) %>%
        arrange(-net_wp_added, -count) %>%
        mutate(
            rank_net_wp = row_number(),
            # total = n()
        ) %>%
        ungroup() %>%
        arrange(season) %>%
        filter(
            pos_team_conference == !!conference
            & season == !!year
        ) %>%
        arrange(-net_wp_added) %>%
        select(-pos_team_conference, -season, -n_unoptimal, -pct_unoptimal, -unoptimal_wp_added,-optimal_wp_added) %>%
        cbbplotR::gt_cbb_teams(pos_team, pos_team, include_name = T) |>
        mutate(
            pos_team = glue::glue("#{row_number()} {pos_team}")
        ) %>%
        gt() %>%
        gt_theme_538() %>%
        tab_header(
            title = md(glue::glue("**{title}**")),
            subtitle = md(subtitle)
        ) %>%
        tab_footnote(
            footnote = "'obvious-go' situation: a fourth down where attempting the conversion has an expected win probability that is 1.5% or better than that of a field goal or punt AND is in the first quarter OR the offense has a win probability above 10%.",
            locations = cells_title(groups = "subtitle")
        ) %>%
        cols_align(columns = c("pos_team"), align = "left") %>%
        cols_align(columns = c("count", "n_optimal"), align = "center") %>%
        cols_align(columns = c("rank_net_wp", "pct_optimal", "net_wp_added"), align = "center") %>%
        fmt_percent(
            columns = pct_optimal,
            drop_trailing_zeros = T,
            decimals = 1
        ) %>%
        fmt_percent(
            columns = net_wp_added,
            decimals = 1,
            scale_values = F,
            force_sign = T
        ) %>%
        fmt_number(
            columns = rank_net_wp,
            decimals = 0,
            pattern = "#{x}"
        ) %>%
        fmt_markdown(pos_team) %>%
        gt_color_rows(net_wp_added, palette = "ggsci::green_material", direction = 1, domain = optimal_wp_ranks$net_wp_added) |> # show how percent can differ to relative performance
        gt_color_rows(rank_net_wp, palette = "ggsci::blue_material", direction = -1, domain = 1:133) |>
        tab_style(
            style = list(
                cell_text(weight = "bold")
            ),
            locations = cells_body(
                columns = c("pos_team"),
            )
        ) %>%
        tab_options(
            heading.align = "left"
        ) %>%
        cols_label(
            # "season" ~ "Season",
            "pos_team" ~ "Team",
            "count" ~  "Obvious-Go Situations",
            "n_optimal" ~ "Chose to Go",
            "pct_optimal" ~ "Chose to Go %",
            "net_wp_added" ~ "Net Win Prob Gained",
            "rank_net_wp" ~ "National Rank",
        ) %>%
        cols_move(net_wp_added, after = pct_optimal) %>%
        tab_source_note(source_note = md(
            "Data from cfbfastR and cfb4th. Logos from cbbdata. Table assembled by @akeaswaran."
        ))  %>%
        # tab_footnote(
        #     footnote = "COVID-shortened season.",
        #     locations = cells_body(columns = season, rows = 7)
        # ) %>%
        opt_css(
            '
      #table .gt_sourcenote {
      line-height: 1.2
      }
      #table .gt_heading {
         padding-bottom: 0px;
         padding-top: 6px
        }
      #table .gt_subtitle {
         padding-top: 2px;
         padding-bottom: 6px;
        }
      '
        )
}

fourth_down_decisions %>%
    analyze_decisions(
        optimal_choice = "Go for it",
        min_wp_diff = 1.5,
        min_wp_before = 0.1,
        max_wp_before = 1.0,
        wp_or_quarter = 1,
        # min_count = 5
    ) %>%
    generate_conference_net_wp_added_graphic("ACC", 2024, md("~~Super~~ Extremely Mid Mario"), "Mario Cristobal and Miami both underperformed the national average in 'obvious-go' rate AND produced such highly volatile outcomes that their decision making had nearly zero net impact.") %>%
    save_crop_gt("net_wp_added_acc.png")

# team historic performance ----

# generate_team_history_table_graphic = function(team, title, subtitle) {

fourth_down_decisions %>%
    analyze_decisions(
        # actual_choice = "Go for it",
        optimal_choice = "Go for it",
        min_wp_diff = 1.5,
        min_wp_before = 0.1,
        max_wp_before = 1.0,
        wp_or_quarter = 1
    ) %>%
    mutate(
        net_wp_added = optimal_wp_added - abs(unoptimal_wp_added)
    ) %>%
    group_by(season) %>%
    arrange(-optimal_wp_added, -count) %>%
    mutate(
        rank_optimal_wp = row_number(),
        # total = n()
    ) %>%
    arrange(-unoptimal_wp_added, -count) %>%
    mutate(
        rank_unoptimal_wp = row_number(),
        # total = n()
    ) %>%
    arrange(-net_wp_added, -count) %>%
    mutate(
        rank_net_wp = row_number(),
        # total = n()
    ) %>%
    arrange(-pct_optimal, -count) %>%
    mutate(
        rank_optimal = row_number(),
        # total = n()
    ) %>%
    arrange(-pct_unoptimal, -count) %>%
    mutate(
        rank_unoptimal = row_number(),
        # total = n()
    ) %>%
    ungroup() %>%
    arrange(season) %>%
    select(season, pos_team, count, n_optimal, pct_optimal, rank_optimal, net_wp_added, rank_net_wp) %>%
    filter(
        # (pos_team == "Western Kentucky" & season %in% 2014:2016)
        # (pos_team == "Purdue" & season %in% 2017:2022)
        # (pos_team == "Louisville" & season >= 2024)
        # pos_team == "Alabama"
        pos_team == "Georgia" & season >= 2016
    ) %>%
    # filter(pos_team == !!team) %>%
    # head(15) %>%
    cbbplotR::gt_cbb_teams(pos_team, pos_team, include_name = T) %>%
    dplyr::mutate(
        pos_team = dplyr::if_else(stringr::str_detect(pos_team, "61.png"), '<img style="margin-bottom: 0; vertical-align: middle;" width="35px" src="https://raw.githubusercontent.com/saiemgilani/game-on-paper-app/main/frontend/public/assets/img/ennui-uga.png"/> georgia', pos_team),
    ) %>%
    gt() %>%
    gt_theme_538() %>%
    tab_header(
        title = md("**kirby smart: Historic Trends**"),
        # subtitle = md("Fickell's teams have been all over the place in terms of 'obvious-go' rate. Even in his most successful seasons at Cincinnati (2020 and 2021), he was not consistent in his decision-making (albeit in a small sample).")
    ) %>%
    tab_footnote(
        footnote = "'obvious-go' situation: a fourth down where attempting the conversion has an expected win probability that is 1.5% or better than that of a field goal or punt AND is in the first quarter OR the offense has a win probability above 10%.",
        locations = cells_column_labels(columns = c("count"))
    ) %>%
    cols_align(columns = c("season", "pos_team"), align = "left") %>%
    cols_align(columns = c("count", "n_optimal"), align = "center") %>%
    cols_align(columns = starts_with("pct_") | starts_with("rank_") | matches("_wp_added"), align = "center") %>%
    fmt_markdown(pos_team) %>%
    # gt_color_rows(columns = starts_with("pct_"), palette = "ggsci::green_material", direction = 1, domain = 0:1) |>
    gt_color_rows(columns = starts_with("rank_"), palette = "ggsci::blue_material", direction = -1, domain = 1:133) |>
    tab_style(
        style = list(
            cell_text(weight = "bold")
        ),
        locations = cells_body(
            columns = c("season", "pos_team"),
        )
    ) %>%
    tab_options(
        heading.align = "left"
    ) %>%
    fmt_percent(
        columns = starts_with("pct_"),
        drop_trailing_zeros = T,
        decimals = 1
    ) %>%
    fmt_number(
        columns = starts_with("rank_"),
        decimals = 0,
        pattern = "#{x}"
    ) %>%
    fmt_percent(
        columns = net_wp_added,
        decimals = 1,
        scale_values = F,
        force_sign = T
    ) %>%
    cols_label(
        "season" ~ "Season",
        "pos_team" ~ "Team",
        "count" ~  "Obvious-Go Situations",
        "n_optimal" ~ "Chose to Go",
        "pct_optimal" ~ "Chose to Go %",
        "rank_optimal" ~ "National Rank",
        "net_wp_added" ~ "Net Win Prob Gained",
        "rank_net_wp" ~ "National Rank",
    ) %>%
    tab_source_note(source_note = md(
        "Data from cfbfastR and cfb4th. Logos from cbbdata. Table assembled by @akeaswaran."
    ))  %>%
    tab_footnote(
        footnote = "COVID-shortened season.",
        locations = cells_body(columns = season, rows = season == 2020)
    ) %>%
    opt_css(
        '
          #table .gt_sourcenote {
          line-height: 1.2
          }
          #table .gt_heading {
             padding-bottom: 0px;
             padding-top: 6px
            }
          #table .gt_subtitle {
             padding-top: 2px;
             padding-bottom: 6px;
            }
      '
    ) %>% save_crop_gt("kirby.png")
# }

