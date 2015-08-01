root = "http://www.parlament.gv.at"
sponsors = "data/sponsors.csv"
bills = "data/bills.csv" # bills

leg = c("XII" = 12, "XIII" = 13, "XIV" = 14, "XV" = 15, "XVI" = 16,
        "XVII" = 17, "XVIII" = 18, "XIX" = 19, "XX" = 20, "XXI" = 21,
        "XXII" = 22, "XXIII" = 23, "XXIV" = 24, "XXV" = 25)

# parse bills (selbständige Anträge)

if(!file.exists(bills)) {
  
  b = data_frame()
  for(i in paste0("XX", c("", "I", "II", "III", "IV", "V"))) {
    
    f = paste0("raw/bill-lists/bills-", i, ".html")
    
    if(!file.exists(f))
      download.file(paste0(root, "/PAKT/RGES/index.shtml?AS=ALLE&GBEZ=&AUS=ALLE&requestId=&ALT=&anwenden=Anwenden&LISTE=&NRBR=NR&RGES=A&FR=ALLE&STEP=&listeId=103&GP=", i, "&SUCH=&pageNumber=&VV=&FBEZ=FP_003&xdocumentUri=%2FPAKT%2FRGES%2Findex.shtml&jsMode="), f, mode = "wb", quiet = TRUE)
    
    h = htmlParse(f)
    t = readHTMLTable(h, stringsAsFactors = FALSE)[[1]][ -1, c(-2, -5) ]
    names(t) = c("date", "title", "ref")
    t$url = unique(xpathSApply(h, "//table[@class='tabelle filter']/*/*/a[contains(@href, '/A/')]/@href"))
    
    b = rbind(cbind(legislature = i, t), b)
    
  }
  b$date = strptime(b$date, "%d.%m.%Y")
  b$sponsors = NA
  write.csv(b, bills, row.names = FALSE)
  
}

b = read.csv(bills, stringsAsFactors = FALSE)

# parse sponsor lists (run twice to solve network issues)

u = b$url[ is.na(b$sponsors) ]
for(i in rev(u)) {
  
  cat(sprintf("%4.0f", which(u == i)), i)
  
  f = gsub("/PAKT/VHG/(\\w+)/A/(.*)/index.shtml", "raw/bill-pages/bill-\\1-\\2.html", i)
  if(!file.exists(f))
    download.file(paste0(root, i), f, mode = "wb", quiet = TRUE)
  
  if(!file.info(f)$size) {
    
    cat(": failed\n")
    file.remove(f)
    
  } else {
    
    h = htmlParse(f)
    j = xpathSApply(h, "//div[@class='c_2']//a[contains(@href, 'WWER')]/@href")
    
    b$sponsors[ b$url == i ] = paste0(gsub("\\D", "", j), collapse=";")
    cat(":", length(j), "sponsor(s)\n")
    
  }

}

# roughly a third of all bills are cosponsored

cat(nrow(b), "bills", sum(grepl(";", b$sponsors)), "sponsored\n")
print(table(b$legislature, grepl(";", b$sponsors)))

write.csv(b, bills, row.names = FALSE)

b$n_au = 1 + str_count(b$sponsors, ";")
b$legislature = leg[ b$legislature ]

# parse sponsors

j = unique(unlist(strsplit(b$sponsors, ";"))) %>% na.omit

k = data_frame()

cat("\nParsing", length(j), "sponsors...\n")
for(i in rev(j)) {
  
  u = paste0(root, "/WWER/PAD_", i, "/index.shtml")
  # cat(sprintf("%4.0f", which(j == i)), u)
  
  f = paste0("raw/mp-pages/mp-", i, ".html")
  if(!file.exists(f))
    try(download.file(u, f, quiet = TRUE), silent = TRUE)
  
  if(!file.info(f)$size)
    file.remove(f)
  
  h = try(htmlParse(f), silent = TRUE)
  
  if(!"try-error" %in% class(h)) {
    
    kreis = xpathSApply(h, "//li[contains(text(), 'Wahlkreis')]", xmlValue)
    kreis = ifelse(!length(kreis), NA, gsub("Wahlkreis: \\d\\w? – ", "", kreis))
    
    federal = xpathSApply(h, "//li[contains(text(), 'Bundeswahlvorschlag')]", xmlValue)
    federal = ifelse(!length(federal), 0, 1)
    
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
    k = rbind(k, unique(data_frame(
      id = paste0("id_", i),
      name, born, sex,
      kreis, federal,
      party, party_full = party,
      mandate, legisl,
      photo = gsub("photos/|\\.jpg", "" , pic)
    )))
    
  } else {
    
    cat(": failed to download photo\n")
    
  }
  
}

# constituencies to Wikipedia handles
k$kreis = gsub("–", "-", k$kreis)
k$kreis = paste0(ifelse(k$kreis %in% c("Burgenland", "Kärnten",
                                       "Niederösterreich", "Oberösterreich",
                                       "Salzburg", "Steiermark", "Tirol",
                                       "Vorarlberg", "Wien"),
                        "Landeswahlkreis", "Regionalwahlkreis"), " ",
                 k$kreis)
k$kreis[ k$kreis == "Regionalwahlkreis Hausruckviertel" ] = "Hausruckviertel"
k$kreis[ k$kreis == "Regionalwahlkreis NA" ] = NA
k$kreis[ k$federal ] = "Bundeswahlvorschlag" # federal-level lists

# fix extra text from replacement of retired MPs and other details
k$party = gsub("(.*)(Eingetret|Mandatszuweisung|Dasdadurch)(.*)", "\\1", k$party)
k$party[ grepl("ohneKlubzugehörigkeit", k$party) ] = "INDEP"
k$party[ nchar(k$party) > 10 ]
k$party_full = k$party

# party fixes (Austrian politicians like to split just before elections...)

# legislature XV (25, 2013-), merged in 2014
k$party[ k$party == "NEOS-LIF" | grepl("^NEOS", k$party) ] = "NEOS"

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

# STRONACH from 2013 election to June 2015, then split to ÖVP
k$party[ k$party == "ÖVP" & k$name == "Dr. Marcus Franz" ] = "STRONACH"

# STRONACH from 2013 election to June 2015, then split to ÖVP
k$party[ k$party == "ÖVP" & k$name == "Dr. Georg Vetter" ] = "STRONACH"

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

# transform mandate years into list
for(i in k$id) {
  # cat(i, ":", length(k$mandate[ k$id == i ]), "rows")
  m = as.numeric(unlist(str_extract_all(k$mandate[ k$id == i ], "[0-9]{4}")))
  if(length(m) == 1)
    m = c(m, 2014)
  # cat(":", paste0(seq(min(m), max(m)), collapse = ";"), "\n")
  # print(subset(k, id == i))
  k$mandate[ k$id == i ] = paste0(seq(min(m), max(m)), collapse = ";")
}

# expand dataset to one row per legislature (for party transitions)
s = data.frame()
for(i in 1:nrow(k)) {
  m = unlist(strsplit(k$legisl[ i ], "–"))
  # stopifnot(all(m %in% names(leg))) ## checked, minimum is XII
  m = seq(from = min(leg[ m ]), to = max(leg[ m ]))
  s = rbind(s, data.frame(k[ i, ], legislature = m, stringsAsFactors = FALSE))
}

s = unique(s[, c("id", "name", "legislature", "party", "mandate", "kreis", "sex", "born", "photo") ])

# after applying party fixes, this yields nothing
for(j in unique(s$legislature)[ unique(s$legislature) > 19 ]) {
  r = subset(s, legislature == j)
  if(sum(duplicated(r$name)))
    print(r[ r$name %in% r$name[ duplicated(r$name) ], ])
}

# kthxbye
