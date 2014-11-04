# parse bills (Selbständige Anträge)

if(!file.exists("data/antrage.csv")) {
  
  bills = data.frame()
  for(i in paste0("XX", c("", "I", "II", "III", "IV", "V"))) {
    
    h = htmlParse(paste0(root, "/PAKT/RGES/index.shtml?AS=ALLE&GBEZ=&AUS=ALLE&requestId=&ALT=&anwenden=Anwenden&LISTE=&NRBR=NR&RGES=A&FR=ALLE&STEP=&listeId=103&GP=", i, "&SUCH=&pageNumber=&VV=&FBEZ=FP_003&xdocumentUri=%2FPAKT%2FRGES%2Findex.shtml&jsMode="))
    t = readHTMLTable(h, stringsAsFactors = FALSE)[[1]][ -1, c(-2, -5) ]
    names(t) = c("date", "title", "ref")
    t$url = unique(xpathSApply(h, "//table[@class='tabelle filter']/*/*/a[contains(@href, '/A/')]/@href"))
    bills = rbind(cbind(legislature = i, t), bills)
    
  }
  bills$date = strptime(bills$date, "%d.%m.%Y")
  bills$sponsors = NA
  write.csv(bills, "data/antrage.csv")
  
}

bills = read.csv("data/antrage.csv", stringsAsFactors = FALSE)

# parse sponsor lists (run twice to solve network issues)

u = bills$url[ is.na(bills$sponsors) ]
for(i in rev(u)) {
  
  cat(sprintf("%4.0f", which(u == i)), i)
  h = try(htmlParse(paste0(root, i)), silent = TRUE)
  if(!"try-error" %in% class(h)) {
    j = xpathSApply(h, "//div[@class='c_2']//a[contains(@href, 'WWER')]/@href")
    bills$sponsors[ bills$url == i ] = paste0(gsub("\\D", "", j), collapse=";")
    cat(":", length(j), "sponsor(s)\n")
  } else {
    cat(": failed\n")
  }
  
}

# roughly a third of all bills are cosponsored

cat(nrow(bills), "bills", sum(grepl(";", bills$sponsors)), "sponsored\n")
print(table(bills$legislature, grepl(";", bills$sponsors)))

write.csv(bills, "data/antrage.csv", row.names = FALSE)

# kthxbye
