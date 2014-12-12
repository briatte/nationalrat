meta = c("Austria", "Nationalrat")
mode = "fruchtermanreingold"

years = c("20" = 1995, "21" = 1999, "22" = 2002, "23" = 2006, "24" = 2008, "25" = 2013)

for(ii in unique(bills$legislature)) {
  
  cat(ii)
  data = subset(bills, legislature == ii & n_au > 1)
  sp = subset(s, legislature == ii)
  
  cat(":", nrow(data), "cosponsored documents, ")
  
  edges = rbind.fill(lapply(data$sponsors, function(d) {
    
    w = paste0("id_", unlist(strsplit(d, ";")))
    
    d = expand.grid(i = sp$name[ sp$id %in% w ],
                    j = sp$name[ sp$id == w[1]], stringsAsFactors = FALSE)
    
    return(data.frame(d, w = length(w) - 1)) # number of cosponsors
    
  }))
  
  #
  # edge weights
  #
  
  # first author self-loops, with counts of cosponsors
  self = subset(edges, i == j)
  
  # count number of bills per first author
  n_au = table(self$j)
  
  # remove self-loops from directed edge list
  edges = subset(edges, i != j)
  
  # count number of bills cosponsored per sponsor
  n_co = table(edges$i)
  
  # identify directed ties
  edges$ij = apply(edges[, 1:2 ], 1, paste0, collapse = "///")
  
  # raw edge counts
  raw = table(edges$ij)
  
  # Newman-Fowler weights (weighted quantity of bills cosponsored)
  edges = aggregate(w ~ ij, function(x) sum(1 / x), data = edges)
  
  # expand to edge list
  edges = data.frame(i = gsub("(.*)///(.*)", "\\1", edges$ij),
                     j = gsub("(.*)///(.*)", "\\2", edges$ij),
                     raw = as.vector(raw[ edges$ij ]), # raw edge counts
                     nfw = edges$w, stringsAsFactors = FALSE)
  
  # Gross-Shalizi weights (weighted propensity to cosponsor)
  edges = merge(edges, aggregate(w ~ j, function(x) sum(1 / x), data = self))
  edges$gsw = edges$nfw / edges$w
  
  # final edge set: cosponsor, first author, weights
  edges = edges[, c("i", "j", "raw", "nfw", "gsw") ]
  
  cat(nrow(edges), "edges, ")
  
  #
  # directed network
  #
  
  n = network(edges[, 1:2 ], directed = TRUE)
  
  n %n% "country" = meta[1]
  n %n% "title" = paste(meta[2], paste0(range(unique(substr(data$date, 1, 4))),
                                        collapse = " to "))
  
  n %n% "n_bills" = nrow(data)
  n %n% "n_sponsors" = table(subset(bills, legislature == ii)$n_au)
  
  n_au = as.vector(n_au[ network.vertex.names(n) ])
  n %v% "n_au" = ifelse(is.na(n_au), 0, n_au)
  
  n_co = as.vector(n_co[ network.vertex.names(n) ])
  n %v% "n_co" = ifelse(is.na(n_co), 0, n_co)
  
  n %v% "n_bills" = n %v% "n_au" + n %v% "n_co"
  
  cat(network.size(n), "nodes\n")
  
  rownames(sp) = sp$name
  
  n %v% "url" = as.character(sp[ network.vertex.names(n), "id" ])
  n %v% "name" = as.character(sp[ network.vertex.names(n), "name" ])
  n %v% "sex" = as.character(sp[ network.vertex.names(n), "sex" ])
  n %v% "born" = as.numeric(substr(sp[ network.vertex.names(n), "born" ], 1, 4))
  n %v% "party" = sp[ network.vertex.names(n), "party" ]
  n %v% "partyname" = as.character(groups[ n %v% "party" ])
  n %v% "lr" = as.numeric(scores[ n %v% "party" ])
  # mandate years done up to start year of legislature
  sp$nyears = sapply(sp$mandate, function(x) {
    sum(unlist(strsplit(x, ";")) <= as.numeric(years[ as.character(ii) ]))
  })
  n %v% "nyears" = as.numeric(sp[ network.vertex.names(n), "nyears" ])
  n %v% "photo" = as.character(sp[ network.vertex.names(n), "photo" ])
  
  set.edge.attribute(n, "source", as.character(edges[, 1])) # cosponsor
  set.edge.attribute(n, "target", as.character(edges[, 2])) # first author
  
  set.edge.attribute(n, "raw", edges$raw) # raw edge counts
  set.edge.attribute(n, "nfw", edges$nfw) # Newman-Fowler weights
  set.edge.attribute(n, "gsw", edges$gsw) # Gross-Shalizi weights
  
  #
  # weighted measures
  #
  
  n = get_modularity(n, weights = "raw")
  n = get_modularity(n, weights = "nfw")
  n = get_modularity(n, weights = "gsw")
  
  n = get_centrality(n, weights = "raw")
  n = get_centrality(n, weights = "nfw")
  n = get_centrality(n, weights = "gsw")
  
  #
  # network plot
  #
    
  if(plot) {
    
    q = n %v% "degree"
    q = as.numeric(cut(q, unique(quantile(q)), include.lowest = TRUE))
    
    ggnet_save(n, file = paste0("plots/net_at", ii),
               i = colors[ sp[ n %e% "source", "party" ] ],
               j = colors[ sp[ n %e% "target", "party" ] ],
               q, colors, order)
    
  }
  
  #
  # save objects
  #
  
  assign(paste0("net_at", ii), n)
  assign(paste0("edges_at", ii), edges)
  assign(paste0("bills_at", ii), data)
  
  #
  # export gexf
  #
  
  if(gexf)
    get_gexf(paste0("net_at", ii), n, meta, mode, colors)
  
}

if(gexf)
  zip("net_at.zip", dir(pattern = "^net_at\\d{2}\\.gexf$"))

save(list = ls(pattern = "^(net|edges|bills)_at\\d{2}$"), file = "data/net_at.rda")
