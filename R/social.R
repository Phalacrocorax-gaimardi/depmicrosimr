#############################
# social influence network
#
#############################


#' make_artificial_society
#'
#' Creates a homophilous social influence network based on demographic attributes contained in *dep_society* and
#' weights in *homophily*.
#'
#' @param society dataframe containing society
#' @param homophily tibble with homophily parameters for society
#' @param nu gower distance exponent
#'
#' @return a tidygraph object
#' @export
#' @importFrom magrittr %>%
#' @examples make_artificial_society(dep_society,homophily,nu=4.5)
#'
make_artificial_society <- function(society=society,homophily=homophily,nu=4.5){
  #create a random homophilous social network
  #social distance measure=gower distance
  #nu gives the social distance decay exponentlarger mu higher assortativity
  #agents with degree zero remain degree zero. Assume that NA indicates degree > 0.
  society <- society %>% dplyr::mutate(serial=as.character(serial))
  #replace NAs with sample distribution, with no additional zeroes
  society <- society %>% dplyr::rowwise() %>% dplyr::mutate(degree = ifelse(is.na(degree), as.numeric(sample(degrees$degree,size=1,prob = degrees$f)),degree))
  society_factor <- unclass(society %>% dplyr::mutate_if(is.character,as.factor)) %>% as.data.frame()
  society_factor1 <- dplyr::filter(society_factor,degree != 0)
  society1 <- dplyr::filter(society, degree != 0)

  N_society1 <- nrow(society1)

  zeronodes <- dplyr::filter(society,degree==0)$serial #nodes with no influencers
  nonzeronodes <- dplyr::filter(society,degree!=0)$serial
  stopifnot(N_society1+length(zeronodes) == dim(society)[1])

  dist_mat <- cluster::daisy(society_factor1[,seq(2,dim(society)[2])], metric ="gower", weights=homophily$weights) %>% as.matrix()
  prob_mat <- (1-dist_mat)^nu
  #prob_mat1 <- 1.1*prob_mat %*% diag(society_factor1$degree/apply(prob_mat,2,sum)) #adjust this parameter


  prob_mat1 <- 1.1*prob_mat %*% diag(society_factor1$degree/apply(prob_mat,2,sum)) #adjust this parameter
  prob_mat1 <- (prob_mat1 + t(prob_mat1)) / 2  #ensure symmetry

  # FIX 4: Clamp probabilities strictly between 0 and 1 to prevent guaranteed random links
  prob_mat1[prob_mat1 > 1] <- 0.95
  diag(prob_mat1) <- 0 # No self-loops

  nodes <- tidyr::tibble(serial=society1$serial)
  edges <- tidyr::expand_grid(from=1:N_society1,to=1:N_society1)
  edges <- dplyr::filter(edges, from < to) #avoid loops
  #edges <- filter(edges, !(from %in% zeronodes)) #no edges frsociom zeronodes
  edges <- edges %>% dplyr::rowwise() %>% dplyr::mutate(p=prob_mat1[from,to])
  edges <- edges %>% dplyr::rowwise() %>% dplyr::mutate(keep=ifelse(stats::runif(1)< p,T,F))
  edges <- edges %>% dplyr::filter(keep)

  edges <- edges[,1:2]
  edges$from <- nodes$serial[edges$from] #relable to orginal ids
  edges$to <- nodes$serial[edges$to]
  #restore zero nodes
  nodes <- dplyr::bind_rows(nodes,tidyr::tibble(serial=zeronodes)) %>% dplyr::arrange(serial)
  #
  nodes <- nodes %>% dplyr::mutate(serial = as.character(serial))
  edges <- edges %>% dplyr::mutate(from = as.character(from), to = as.character(to))

  #restore
  g <- tidygraph::tbl_graph(nodes=nodes,edges=edges,directed=F,node_key="serial") %>% dplyr::inner_join(society,by="serial")
  return(g)

}

#dep_soc <- dep_society %>% dplyr::mutate(degree = ifelse(is.na(degree), as.numeric(sample(degrees$degree,size=1,prob = degrees$f)),degree))

#g <- make_artificial_society(dep_soc,homophily,4.5)
#high correlation between computed degree and stated degree
#cor(igraph::degree(g),g %>% tibble::as_tibble() %>% dplyr::pull(degree)) #correlation is 80%
#plot(igraph::degree(g), dep_soc$degree)


#' get_network_characteristics
#'
#' @param society society used to construct homophilous network
#' @param g homophilous social network
#'
#' @return table
#' @export
#'
#' @examples
#'
get_network_characteristics <- function(society, g){


  # 1. Pull the nodes directly out of the graph object 'g' to guarantee
  # they are in the exact structural order igraph expects.
  graph_nodes <- g %>% tidygraph::activate(nodes) %>% tibble::as_tibble()

  # 2. Map through your homophily table using the graph-ordered data
  homophily1 <- homophily %>%
    dplyr::filter(variable != "degree") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(assortativity = {

      # Pull the vector using the dynamic string name from graph_nodes
      node_vector <- graph_nodes[[variable]]

      # Convert to factor safely (treating NAs cleanly) and then to integer
      node_integers <- as.integer(factor(node_vector, exclude = NULL))

      # Calculate assortativity using the aligned vector
      igraph::assortativity_nominal(g, node_integers)
    }) %>%
    dplyr::ungroup()

  # Render the clean table
  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop(
      "The 'knitr' package is required table rendering",
      call. = FALSE
    )
  }

  if (!requireNamespace("kableExtra", quietly = TRUE)) {
    stop(
      "The 'kableExtra' package is required table rendering ",
      "Please install it using install.packages('kableExtra')",
      call. = FALSE
    )
  }

  homophily1 %>%
    knitr::kable() %>%
    kableExtra::kable_styling()
}


#get_network_characteristics(dep_soc,g)

#igraph::transitivity(g,type="global") #low transitivity ... need clique models

#social influence network compatible with

#df <- tibble()
#for(nu in seq(0.1,12,by=0.1)){

#   g <- make_artificial_society(dep_society,homophily,nu=nu)
#   df <- bind_rows(df, tibble(nu=nu, deg = mean(igraph::degree(g))))#, simil_areatype = assortativity(g,factor(society$area_type)),simil_education = assortativity(g,factor(society$education))))
#}

#df_0 %>% ggplot( aes(deg,nu)) + geom_point() + geom_vline(xintercept=mean(pv_society_oo$degree)) + geom_smooth()+scale_x_continuous(breaks=0:12)
# observed degrees fixes nu=4.5
#df %>% select(-deg) %>% pivot_longer(-nu) %>% ggplot( aes(nu,value,colour=name)) + geom_point()




