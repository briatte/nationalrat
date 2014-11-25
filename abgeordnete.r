# parse sponsors

j = unique(unlist(strsplit(bills$sponsors, ";")))

if(!file.exists(sponsors)) {
  k = data.frame()
} else {
  k = read.csv(sponsors, stringsAsFactors = FALSE)
  j = j[ !j %in% k$id ]
}

cat("\nParsing", length(j), "sponsors...\n")
for(i in rev(j)) {
  
  u = paste0(root, "/WWER/PAD_", i, "/index.shtml")
  # cat(sprintf("%4.0f", which(j == i)), u)
  
  f = paste0("raw/", i, ".html")
  if(!file.exists(f))
    try(download.file(u, f, quiet = TRUE), silent = TRUE)
  
  if(!file.info(f)$size)
    file.remove(f)

  h = try(htmlParse(f), silent = TRUE)
  
  if(!"try-error" %in% class(h)) {
    
    name = xpathSApply(h, "//h1[@id='inhalt']", xmlValue)
    
    born = xpathSApply(h, "//div[@class='rechteSpalte60']/p[1]", xmlValue)
    born = gsub("Geb\\.: \\d{1,2}\\.\\d{1,2}\\.(\\d{4})(.*)", "\\1", born)
    
    nfo = xpathSApply(h, "//div[@class='rechteSpalte60']/ul/li", xmlValue)
    nfo = nfo[ grepl("^(\\n+)?Abgeordnete(r)? zum Nationalrat", nfo) ]
    
    sex = ifelse(grepl("Abgeordneter", nfo), "M", "F")
    
    party = gsub("\\d|\\.|\\s|–", "", gsub("(.*), (.*)", "\\2", nfo))
    
    mandate = str_extract(nfo, "[0-9]{2}\\.[0-9]{2}\\.[0-9]{4}( – )?([0-9]{2}\\.[0-9]{2}\\.[0-9]{4})?")
    legisl = gsub("\\(|\\)|GP|\\.\\s", "", str_extract(nfo, "\\((.*) GP\\)"))
    
    # photo
    photo = unique(xpathSApply(h, "//div[contains(@class, 'teaserPortraitLarge')]//img[contains(@src, 'WWER')]/@src"))
    if(!is.null(photo)) {
      
      pic = gsub("_WWER_PAD_|\\.jpg", "", gsub("/", "_", photo))
      pic = unique(paste0("photos/", gsub("_180$|_384$", "", pic), ".jpg"))
      # if(length(photo) > 1)
      #  print(photo)
      
      if(!file.exists(pic))
        h = try(download.file(paste0(root, photo), pic, mode = "wb", quiet = TRUE), silent = TRUE)
      # else
      #  cat("\n")

      if("try-error" %in% class(h) | !file.info(pic)$size) {
      #  cat(":: pic failed\n")
        file.remove(pic)
        pic = NA
      }
      
    } else {
      
      # cat(": no photo found\n")
      pic = NA
      
    }
    
    ## cat(":", name, "\n")
    k = rbind(k, unique(data.frame(id = paste0("id_", i),
                                   name, born, sex, party, party_full = party, mandate, legisl,
                                   photo = gsub("photos/|\\.jpg", "" , pic))))
    
  } else {
    
    cat(": failed to download photo\n")
    
  }
  
}

# fix extra text from replacement of retired MPs and other details
k$party = gsub("(.*)(Eingetret|Mandatszuweisung|Dasdadurch)(.*)", "\\1", k$party)
k$party[ grepl("ohneKlubzugehörigkeit", k$party) ] = "INDEP"
k$party[ nchar(k$party) > 10 ]
k$party_full = k$party

# party fixes (Austrian politicians like to split just before elections...)

# legislature XV (25, 2013-), merged in 2014
k$party[ k$party == "NEOS-LIF" ] = "NEOS"

# legislature XXII (22, 2002-2006-), split a few months before 2006 election
k$party[ k$party == "F-BZÖ" ] = "F"

# split a few months bef. 2013 elec.
k$party[ grepl("STRONACH|INDEP", k$party) & grepl("Christoph Hagen|Stefan Markowitz", k$name) ] = "BZÖ"

# BZÖ, independent for a few months, then BZÖ again
k$party[ k$party == "INDEP" & k$name == "Gerhard Huber" ] = "BZÖ"

# F/FPÖ splitters, independents for only two months in mid-2008, in time for election
k$party[ k$party == "INDEP" & k$name == "DDr. Werner Königshofer" ] = "FPÖ"
k$party[ k$party == "INDEP" & k$name == "Dipl.-Ing. Karlheinz Klement, MAS" ] = "FPÖ"

# F/FPÖ splitter, independent for four months and then quit
k$party[ k$party == "INDEP" & k$name == "Peter Rosenstingl" ] = "F"
# k$party[ k$party == "F" & k$name == "Peter Rosenstingl" ] = "FPÖ" # leg. XX

# F/FPÖ and then BZÖ, independent for only two months while transiting
k$party[ k$party == "INDEP" & k$name == "Mag. Ewald Stadler" ] = "FPÖ"

# simplified situation: L, and then F one year after 1996 election
k$legisl[ k$party == "F" & k$name == "Mag. Reinhard Firlinger" ] = "XXI"

# simplified situation: BZÖ, then FPÖ a few months before 2013 election
k$legisl[ k$party == "FPÖ" & k$name == "Mag. Gernot Darmann" ] = "XXV"

# small bug for Dr. Christoph Matznetter
k$party[ k$party == "MAS" ] = "SPÖ"

# final simplifications, merge F and FPÖ...
k$party[ k$party == "F" ] = "FPÖ"
k$party[ k$party == "L" ] = "LIF" # ... and use three-letter abbr. for LiF

k$legisl = gsub("\\.", "", k$legisl)

k = unique(k)
write.csv(k, sponsors, row.names = FALSE)

s = data.frame()
for(i in 1:nrow(k)) {
  m = unlist(strsplit(k$legisl[ i ], "–"))
  # stopifnot(all(m %in% names(leg))) ## checked, minimum is XII
  m = seq(from = min(leg[ m ]), to = max(leg[ m ]))
  s = rbind(s, data.frame(k[ i, ], legislature = m, stringsAsFactors = FALSE))
}

s = unique(s[, c("id", "name", "legislature", "party", "sex", "born", "photo") ])

# imputed seniority
s = ddply(s, .(id), transform, nyears = 5 * 1:length(id))

# after applying party fixes, this yields nothing
for(j in unique(s$legislature)[ unique(s$legislature) > 19 ]) {
  r = subset(s, legislature == j)
  if(sum(duplicated(r$name)))
    print(r[ r$name %in% r$name[ duplicated(r$name) ], ])
}

# last visual check
# View(reshape::sort_df(s, c("name", "legislature")))

# party names
s$partyname = NA
s$partyname[ s$party == "BZÖ" ] = "BZÖ – Bündnis Zukunft Österreich"
s$partyname[ s$party == "FPÖ" ] = "FPÖ – Freiheitliche Partei Österreichs"
s$partyname[ s$party == "GRÜNE" ] = "Die Grünen"
s$partyname[ s$party == "LIF" ] = "LiF – Liberales Forum"
s$partyname[ s$party == "NEOS" ] = "NEOS – Das Neue Österreich"
s$partyname[ s$party == "ÖVP" ] = "ÖVP – Österreichische Volkspartei"
s$partyname[ s$party == "SPÖ" ] = "SPÖ – Sozialdemokratische Partei Österreichs"
s$partyname[ s$party == "STRONACH" ] = "Team Stronach"
