#' Tree Vizualization
#'
#' Draw lines on a scatter plot to visualize tree decisions
#' @param data \ta data frame whose first 3 columns are in the form:
#'  Outcome, Feature 1, Feature 2
#' @param tree \ta classification or regression tree made with the rpart package
#' @return a ggplot object
#' @examples
#' # This example uses the mnist dataset from the dslabs packages
#' library(dslabs)
#' data(mnist_27)
#' train27<-mnist_27$train
#' classTree<- rpart(y~x_1+x_2, data = train27, method = "class")
#' viz_tree(train27, classTree)
#' @export


viz_tree <- function(data, tree) {
    require(ggplot2)
  tree_string <- capture.output(tree)
  tree_string <- tree_string[7:length(tree_string)]
  node_list <- list()
  k <- 1
  for (line in tree_string){
    value <- as.numeric(regmatches(line, regexpr("\\d+\\.\\d+", line)))
    var_name <- trimws(substr(line, regexpr("\\)", line) + 1,
     regexpr("[><=]", line) - 1))
    indent_lvl <- nchar(sub("\\).*", "", line))
    less <- substr(line, regexpr("[><]", line), regexpr("[><]", line)) == "<"
    node <- list(indent_lvl = indent_lvl, value = value,
     var_name = var_name, less = less)
    node_list[[k]] <- node
    k <- k + 1
  }
  # this code normalizes the indent
  ranks <- unique(lapply(node_list, "[[", 1))
  for (i in seq_along(ranks)){
    for (j in seq_along(node_list)){
      if (node_list[[j]]$indent_lvl == ranks[[i]]) {
        node_list[[j]]$indent_lvl <- i
      }
    }
  }
  tree_plot <- ggplot(data, aes(x = data[[2]], y = data[[3]],
   color = as.factor(data[[1]]))) + geom_point() +
    labs(x = colnames(data[2]), y = colnames(data[3]),
     color = colnames(data[1]))
  y_range <- layer_scales(tree_plot)$y$range$range
  x_range <- layer_scales(tree_plot)$x$range$range
  lines <- list()
  x_var <- node_list[[1]]$var_name
  x <- x_range[1]
  xend <- x_range[2]
  y <- y_range[1]
  yend <- y_range[2]
  indent <- list()
  for (i in seq_along(ranks)){
    indent[[i]] <- list(x = x, xend = xend, y = y, yend = yend)
  }
  max_indent <- length(indent)
  k <- 1
  for (i in seq_along(node_list)){
    if (node_list[[i]]$less == TRUE) {
      cur_indent <- node_list[[i]]$indent_lvl
      if (node_list[[i]]$var_name == x_var) { # vertical line
        lines[[k]] <- list(x = node_list[[i]]$value,
         xend = node_list[[i]]$value,
         y = indent[[cur_indent]]$y,
         yend = indent[[cur_indent]]$yend)
        if (cur_indent + 1 <= max_indent) {
          for (j in seq(cur_indent + 1, max_indent, 1)){
            indent[[j]]$xend <- node_list[[i]]$value
          }
        }
      }else { # horizontal line
        lines[[k]] <- list(x = indent[[cur_indent]]$x,
         xend = indent[[cur_indent]]$xend,
         y = node_list[[i]]$value,
          yend = node_list[[i]]$value)
        if (cur_indent + 1 <= max_indent) {
          for (j in seq(cur_indent + 1, max_indent, 1)){
            indent[[j]]$yend <- node_list[[i]]$value
          }
        }
      }
    }else {
      k <- k - 1
      if (node_list[[i]]$var_name == x_var) { # vertical line
        if (cur_indent < max_indent) {
          for (j in seq(cur_indent + 1, max_indent, 1)){
            indent[[j]]$x <- indent[[j]]$xend
            indent[[j]]$xend <- indent[[j - 1]]$xend
          }
        }
      }else { # horizontal line
        if (cur_indent < max_indent) {
          for (j in seq(cur_indent + 1, max_indent, 1)){
            indent[[j]]$y <- indent[[j]]$yend
            indent[[j]]$yend <- indent[[j - 1]]$yend
          }
        }
      }
    }
    k <- k + 1
  }
  for (i in seq_along(lines)){
    tree_plot <- tree_plot + geom_segment(aes_(x = lines[[i]]$x,
     xend = lines[[i]]$xend, y = lines[[i]]$y,
     yend = lines[[i]]$yend), color = "black")
  }
  return(tree_plot)
}