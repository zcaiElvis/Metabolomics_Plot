#' Plot the stacked barplot representing the abundance
#'
#' @param ion_count ion count data frame
#' @param iso_abun mass isotopomer data frame. First column contains all Fragment
#' names
#' @param group_length Number of experiments in each repetition. First column contains
#' names of ions
#' @param total_length Number of all experiments
#'
#' @return plots stored in output folder
#' @export
#'
#' @examples
plot_abundance <- function(ion_count, iso_abun,
                           group_length = 8, total_length = 24){
  ions <- iso_abun$Ion
  # Add identifying columns
  ion_count$Fragment_type <- split_string_take_first(ion_count$Fragment, by = " ")
  iso_abun$Ion_type <- split_string_take_first(iso_abun$Ion, by = " ")
  iso_abun$Ion_type <- unlist(lapply(iso_abun$Ion_type,
                                     function(x) substr(x, 1, nchar(x)-3)))

  ion_type <- iso_abun$Ion_type

  # Multiply ion_count and iso_abun dfs
  iso_abun <- multiply_dfs(ion_count, iso_abun)

  # Aggregate dfs
  aggr_result <- aggregate_dfs(iso_abun, group_length, total_length)
  aggr_mean <- aggr_result$aggr_mean
  aggr_sem <- aggr_result$aggr_sem

  # Plot
  na_index <- which(is.na(aggr_mean$V1))
  start_index <- c(1, (na_index + 1))
  end_index <- c((na_index - 1), nrow(aggr_mean))
  default_palette = scales::hue_pal()(20)


  for(k in 1:length(start_index)){
    # Sub-setting dataframe
    k_start <- start_index[k]
    k_end <- end_index[k]
    df_iso_mean <- aggr_mean[k_start:k_end, 1:ncol(aggr_mean)]
    df_iso_sem <- aggr_sem[k_start:k_end, 1:ncol(aggr_sem)]

    # Create group name: M0, M1, ...
    m_size <- k_end - k_start + 1
    group_name <- as.character(0:(m_size - 1)) # Starts with M0
    group_name <- unlist(lapply(group_name, function (x) paste0("M", x)))
    color_used <- rev(default_palette[1:m_size])

    # Reshape/flatten dataframes
    df_iso_mean <- reshape2::melt(df_iso_mean) %>%
      dplyr::mutate(Group = rep(group_name, group_length))

    df_iso_sem <- reshape2::melt(df_iso_sem) %>%
      dplyr::mutate(Group = rep(group_name, group_length))

    # Summarize data into dataframe to plot for
    df_to_plot <- df_iso_mean

    df_to_plot <- df_to_plot %>%
      dplyr::mutate(sd = df_iso_sem$value)%>%
      dplyr::group_by(variable) %>%
      dplyr::mutate(Group = factor(Group, levels = rev(group_name))) %>%
      dplyr::mutate(sd_pos = cumsum(value))

    # Don't plot error bars for negative values
    df_to_plot$sd[which(df_to_plot$value < 0)] <- NA
    df_to_plot$sd_pos[which(df_to_plot$value < 0)] <- NA


    # Plot
    p <- ggplot2::ggplot(df_to_plot,
                         ggplot2::aes(x = variable, y = value, fill = Group))+
      ggplot2::geom_bar(position="stack", stat = "identity") +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = sd_pos - sd, ymax = sd_pos + sd,
                                          width = 0.3))+
      ggplot2::theme_bw()+
      ggplot2::ylab("Ion counts")+
      ggplot2::ggtitle(ion_type[start_index[k]])+
      ggplot2::scale_fill_manual(values = color_used)

    ggplot2::ggsave(paste0("output/", ion_type[start_index[k]], ".png"))
  }
}


#' Multiply the ion_count and iso_abun dfs
#'
#' @inheritParams plot_abundance
#' @return df of the multiplication result
#' @export
#'
#' @examples
multiply_dfs <- function(ion_count, iso_abun){
  for(i in 1:nrow(ion_count)){
    # selecting corresponding Ion counts
    frag_count <- ion_count[i,]
    frag_count <- dplyr::select(frag_count, -c("Fragment", "Fragment_type"))
    frag <- ion_count$Fragment_type[i]

    # find matching isotopomer
    abun_mul_loc <- which(iso_abun$Ion_type == frag)

    # multiply
    for(loc in abun_mul_loc){
      iso_abun[loc, 2:(ncol(iso_abun)-2)] <- # TODO: remove hard coding 2 here
        iso_abun[loc, 2:(ncol(iso_abun)-2)] * frag_count
    }
  }
  iso_abun
}

#' Aggregate the iso abundance dataframes by mean and sem
#'
#' @param iso_abun
#'
#' @return
#' @export
#' @inheritParams plot_abundance
#'
#' @examples
aggregate_dfs <- function(iso_abun, group_length, total_length){
  iso_abun <- dplyr::select(iso_abun, -c("Ion", "Ion_type", "Theory"))
  num_subs <- total_length/group_length
  df_start_col_index <- seq(1, total_length, group_length)

  aggr_mean <- matrix(rep(0, nrow(iso_abun)*group_length),
                      nrow = nrow(iso_abun), ncol = group_length)
  aggr_sem <- aggr_mean

  for(i in 1:nrow(iso_abun)){
    counter = 0
    for(j in 1:group_length){
      rep_entry <- unlist(iso_abun[i, (df_start_col_index + counter)])
      aggr_mean[i, j] = mean(rep_entry)
      aggr_sem[i, j] = sd(rep_entry)/sqrt(num_subs)
      counter = counter + 1
    }
  }

  aggr_mean <- as.data.frame(aggr_mean)
  aggr_sem <- as.data.frame(aggr_sem)

  list(aggr_mean = aggr_mean, aggr_sem = aggr_sem)

}



#' Splitting strings in a list of strings by a character, and
#' taking the first component
#'
#' @param l list of strings
#' @param by character to split strings by
#'
#' @return List of strings
#' @export
#'
#' @examples
#' split_string_take_first(c("abc def", "ghi, jkl"))
split_string_take_first <- function(l, by = " "){
  l_splitted <- stringr::str_split(l, by)
  unlist(lapply(l_splitted, function(x) x[1]))
}
