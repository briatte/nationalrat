
years = c("20" = "1995", "21" = "1999", "22" = "2002", "23" = "2006",
          "24" = "2008", "25" = "2013", "26" = "2018")

for (ii in b$legislature %>% unique %>% sort) {
  
  cat(ii)
  
  data = subset(b, legislature == ii & n_au > 1)
  sp = subset(s, legislature == ii)
  
  u = unlist(strsplit(data$sponsors, ";"))
  stopifnot(paste0("id_", u) %in% sp$id)
  
  cat(":", nrow(data), "cosponsored documents, ")
  
  edges = bind_rows(lapply(data$sponsors, function(d) {
    
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
  edges = data_frame(i = gsub("(.*)///(.*)", "\\1", edges$ij),
                     j = gsub("(.*)///(.*)", "\\2", edges$ij),
                     raw = as.vector(raw[ edges$ij ]), # raw edge counts
                     nfw = edges$w)
  
  # Gross-Shalizi weights (weighted propensity to cosponsor)
  edges = merge(edges, aggregate(w ~ j, function(x) sum(1 / x), data = self))
  edges$gsw = edges$nfw / edges$w
  
  # sanity check
  stopifnot(edges$gsw <= 1)
  
  # final edge set: cosponsor, first author, weights
  edges = select(edges, i, j, raw, nfw, gsw)
  
  cat(nrow(edges), "edges, ")
  
  #
  # directed network
  #
  
  n = network(edges[, 1:2 ], directed = TRUE)
  
  n %n% "country" = meta[ "cty" ] %>% as.character
  n %n% "lang" = meta[ "lang" ] %>% as.character
  n %n% "years" = paste0(years[ as.character(ii) ], "-", years[ as.character(ii + 1) ])
  n %n% "legislature" = ii
  n %n% "chamber" = meta[ "ch" ] %>% as.character
  n %n% "type" = meta[ "type" ] %>% as.character
  n %n% "ipu" = meta[ "ipu" ] %>% as.integer
  n %n% "seats" = meta[ "seats" ] %>% as.integer
  
  n %n% "n_cosponsored" = nrow(data)
  n %n% "n_sponsors" = table(subset(b, legislature == ii)$n_au)
  
  n_au = as.vector(n_au[ network.vertex.names(n) ])
  n %v% "n_au" = ifelse(is.na(n_au), 0, n_au)
  
  n_co = as.vector(n_co[ network.vertex.names(n) ])
  n %v% "n_co" = ifelse(is.na(n_co), 0, n_co)
  
  n %v% "n_bills" = n %v% "n_au" + n %v% "n_co"
  
  cat(network.size(n), "nodes\n")
  
  rownames(sp) = sp$name
  
  n %v% "url" = sp[ network.vertex.names(n), "url" ]
  n %v% "sex" = sp[ network.vertex.names(n), "sex" ]
  n %v% "born" = sp[ network.vertex.names(n), "born" ]
  n %v% "party" = sp[ network.vertex.names(n), "party" ]
  n %v% "partyname" = groups[ n %v% "party" ] %>% as.character
  n %v% "constituency" = sp[ network.vertex.names(n), "kreis" ]
  n %v% "lr" = scores[ n %v% "party" ] %>% as.numeric
  # mandate years done up to start year of legislature
  sp$nyears = sapply(sp$mandate, function(x) {
    sum(unlist(strsplit(x, ";")) <= as.numeric(years[ as.character(ii) ]))
  })
  n %v% "nyears" = sp[ network.vertex.names(n), "nyears" ] %>% as.integer
  n %v% "photo" = sp[ network.vertex.names(n), "photo" ]
  
  set.edge.attribute(n, "source", as.character(edges[, 1])) # cosponsor
  set.edge.attribute(n, "target", as.character(edges[, 2])) # first author
  
  set.edge.attribute(n, "raw", edges$raw) # raw edge counts
  set.edge.attribute(n, "nfw", edges$nfw) # Newman-Fowler weights
  set.edge.attribute(n, "gsw", edges$gsw) # Gross-Shalizi weights
    
  #
  # network plot
  #
    
  if (plot) {
    
    save_plot(n, paste0("plots/net_at", years[ as.character(ii) ], "-", years[ as.character(ii + 1) ]),
              i = colors[ sp[ n %e% "source", "party" ] ],
              j = colors[ sp[ n %e% "target", "party" ] ],
              mode, colors)
    
  }
  
  #
  # save objects
  #
  
  assign(paste0("net_at", years[ as.character(ii) ]), n)
  assign(paste0("edges_at", years[ as.character(ii) ]), edges)
  assign(paste0("bills_at", years[ as.character(ii) ]), data)
  
  #
  # export gexf
  #
  
  if (gexf)
    save_gexf(n, paste0("net_at", years[ as.character(ii) ], "-", years[ as.character(ii + 1) ]), 
              mode, colors)
  
}

if (gexf)
  zip("net_at.zip", dir(pattern = "^net_at\\d{4}-\\d{4}\\.gexf$"))

save(list = ls(pattern = "^(net|edges|bills)_at\\d{4}$"), 
     file = "data/net_at.rda")
