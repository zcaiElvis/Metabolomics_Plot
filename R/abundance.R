

#' Title
#'
#' @param col_groups
#' @param ion_count
#' @param iso_abun
#' @param save_loc
#' @param group_length
#' @param total_length
#' @param xtick
#' @param offset
#' @param removed_cols
#' @param offset
#'
#' @return
#' @export
#'
#' @examples
plot_multi_abundance <- function(col_groups, ion_count, iso_abun, folder_name,
                                 group_length = 8, total_length = 24,
                                 xtick = NA,
                                 plot_offset = NA){

  # offsets for the first two are 37, 38. plot_offset = list(c(37, 38), c(37, 38))

  for(i in 1:col_groups){
    group_index <- (2:(total_length + 1)) + (i - 1)*total_length
    ion_count_sub <- ion_count[c(1, group_index)]
    iso_abun_sub <- iso_abun[c(1, group_index, ncol(iso_abun))]

    offsets <- unlist(plot_offset[i])
    offsets <- iso_abun[,offsets, drop = T]


    save_loc <- paste0(folder_name, as.character(i), "/")

    plot_abundance(ion_count_sub, iso_abun_sub, save_loc = save_loc, group_length,
                   total_length, offset = offsets)
  }

}



#' Plot the stacked barplot of abundance
#'
#' @param ion_count ion count data frame
#' @param iso_abun mass isotopomer data frame. First column contains all Fragment
#' names
#' @param group_length Number of experiments in each repetition
#' @param total_length Total number of experiments in all repetition
#' @param xtick Name of variables on the x axis
#' @param offset (Not used for now) Can be index or vector of values.
#' @param removed_cols list of columns to be removed
#' @param save_loc folder names to store output, in folder `Output`
#' @return plots stored in output folder
#' @export
#'
#' @examples
plot_abundance <- function(ion_count, iso_abun, save_loc,
                           group_length = 8, total_length = 24,
                           xtick = NA,
                           offset = NA,
                           removed_cols = NA){


  ions <- iso_abun$Ion
  # Add identifying columns
  ion_count$Fragment_type <- split_string_take_first(ion_count$Fragment, by = " ")
  iso_abun$Ion_type <- split_string_take_first(iso_abun$Ion, by = " ")
  iso_abun$Ion_type <- unlist(lapply(iso_abun$Ion_type,
                                     function(x) substr(x, 1, nchar(x)-3)))

  ion_type <- iso_abun$Ion_type


  # Multiply ion_count and iso_abun dfs
  multiplied_dfs <- multiply_dfs(ion_count, iso_abun)



  # Aggregate dfs
  aggr_result <- aggregate_dfs(multiplied_dfs, group_length, total_length, NA,
                               removed_cols) # TODO: subtracting offset is set as NA
  aggr_mean <- aggr_result$aggr_mean
  aggr_sem <- aggr_result$aggr_sem


  # Plot
  na_index <- which(is.na(aggr_mean$V1))
  start_index <- c(1, (na_index + 1))
  end_index <- c((na_index - 1), nrow(aggr_mean))
  default_palette <- RColorBrewer::brewer.pal(n = 7, name = 'Pastel1')
  default_palette <- c("#e6e6e6", default_palette)
  default_palette <- c(default_palette, rep("#7b797d", 12))


  for(k in 1:length(start_index)){
    # Sub-setting dataframe
    k_start <- start_index[k]
    k_end <- end_index[k]
    df_iso_mean <- aggr_mean[k_start:k_end, 1:ncol(aggr_mean)]
    df_iso_sem <- aggr_sem[k_start:k_end, 1:ncol(aggr_sem)]

    # df_offset <- offset[k_start:k_end, 1:ncol(offset)]
    # print(df_offset)

    # Create group name: M0, M1, ...
    m_size <- k_end - k_start + 1
    group_name <- as.character(0:(m_size - 1)) # Starts with M0
    group_name <- unlist(lapply(group_name, function (x) paste0("M+", x)))
    color_used <- rev(default_palette[1:m_size])

    # Reshape/flatten dataframes
    df_iso_mean <- reshape2::melt(df_iso_mean) %>%
      dplyr::mutate(Group = rep(group_name, group_length))

    df_iso_sem <- reshape2::melt(df_iso_sem) %>%
      dplyr::mutate(Group = rep(group_name, group_length))

    # df_offset <- reshape2::melt(df_offset) %>%
    #   dplyr::mutate(Group = rep(group_name, ncol(df_offset)))
    #
    # print(df_offset)

    # Summarize data into dataframe to plot for
    df_to_plot <- df_iso_mean

    df_to_plot <- df_to_plot %>%
      dplyr::mutate(sd = df_iso_sem$value)%>%
      dplyr::group_by(variable) %>%
      dplyr::mutate(Group = factor(Group, levels = rev(group_name))) %>%
      dplyr::mutate(value = replace(value, value < 0, 0)) %>%
      dplyr::mutate(sd_pos = cumsum(value))


    # Don't plot error bars for negative values
    df_to_plot$sd[which(df_to_plot$value == 0)] <- NA
    df_to_plot$sd_pos[which(df_to_plot$value == 0)] <- NA

    # df_offset <- df_offset %>%
    #   dplyr::mutate(sd = NA) %>%
    #   dplyr::mutate(sd_pos = NA)

    # df_to_plot <- rbind(df_to_plot, df_offset)

    # xtick
    if(!any(is.na(xtick))){
      current_varnames <- unlist(lapply(1:group_length, function(x) paste0("V", as.character(x))))
      new_varnames <- set_names(as.list(xtick), current_varnames)
      df_to_plot$variable <- unlist(lapply(df_to_plot$variable, function(x) new_varnames[x]))
      df_to_plot$variable <- factor(df_to_plot$variable, levels = xtick)
    }

    # Plot
    p <- ggplot2::ggplot(df_to_plot,
                         ggplot2::aes(x = variable, y = value, fill = Group))+
      ggplot2::geom_bar(position="stack", stat = "identity") +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = sd_pos - sd, ymax = sd_pos + sd,
                                          width = 0.3))+
      # ggplot2::geom_point()+
      ggplot2::theme_bw()+
      ggplot2::ylab("Ion percentage")+
      ggplot2::scale_fill_manual(values = color_used)+
      ggplot2::labs(title = ion_type[start_index[k]])+
      ggplot2::theme(plot.title = ggplot2::element_text(size = 30),
                     axis.text.x = ggplot2::element_text(angle = 90, hjust=1))



    ggplot2::ggsave(paste0("output/", save_loc, ion_type[start_index[k]], ".png"))

  }

  return(multiplied_dfs)

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
aggregate_dfs <- function(iso_abun, group_length, total_length, offset, removed_cols){
  iso_abun <- dplyr::select(iso_abun, -c("Ion", "Ion_type", "Theory"))
  num_subs <- total_length/group_length
  df_start_col_index <- seq(1, total_length, group_length)

  if (!all(is.na(offset))) {
    if (length(offset) < nrow(iso_abun)) { # offset is given as index
      offset_values <- iso_abun[, offset, drop = TRUE]
    } else if (length(offset) == nrow(iso_abun)){ # off set is given as values
      offset_values <- offset
    }
    stop("Error in offset")
  }

  aggr_mean <- matrix(rep(0, nrow(iso_abun)*group_length),
                      nrow = nrow(iso_abun), ncol = group_length)
  aggr_sem <- aggr_mean

  aggr_pt <- as.data.frame(matrix(0, nrow = nrow(iso_abun), ncol = group_length))

  # TODO: make list of removed_cols into tuples (a, b), a is repetition, b is
  # exp within the repetition

  for(i in 1:nrow(iso_abun)){
    counter = 0
    for(j in 1:group_length){
      rep_index <- df_start_col_index + counter
      rep_entry <- unlist(iso_abun[i, rep_index])


      # Check offset colu
      if (!all(is.na(offset))) rep_entry - offset_values[i]
      if (!all(is.na(removed_cols))) {
        check_match <- match(removed_cols, rep_index)
        check_match <- check_match[!is.na(check_match)]
        if (length(check_match) != 0) {
          rep_entry <- rep_entry[-check_match]

        }
      }

      aggr_mean[i, j] = mean(rep_entry)
      aggr_sem[i, j] = sd(rep_entry)/sqrt(length(rep_entry))
      # aggr_pt[i, j] = list(rep_entry)
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
