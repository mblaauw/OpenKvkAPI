getCompanies <- function ( filename = 'data/input_companies.txt' ) {
  # INIT
  library(RJSONIO)
  
  #list of input companies
  data.lookup_companies = read.csv(file=filename, encoding='utf-8')
  
  #loop through amount of companies to lookup
  for (i in 1:nrow(data.lookup_companies)) {
    
    # generate URL
    url = paste('http://officieel.openkvk.nl/json/',data.lookup_companies[i,],sep='')
    
    #debug
    print(url)
    
    # Collect JSON
    # TODO: Add logic for when results are not returned
    data.openkvk_result = fromJSON(url,encoding='utf-8',nullValue='NA') 
    
    # Parse JSON (warning N results 1 to N columns)
    returned_records = length(data.openkvk_result)

    
    for (j in 1:returned_records) {
      tmp.result = data.frame(t(unlist(data.openkvk_result[j])), stringsAsFactors=FALSE)
      
      rechtspersoon         = if (is.null(tmp.result$rechtspersoon) == TRUE) { 'NA' } else { tmp.result$rechtspersoon } 
      vestigingsnummer      = if (is.null(tmp.result$vestigingsnummer) == TRUE) { 'NA' } else { tmp.result$vestigingsnummer }
      adres                 = if (is.null(tmp.result$adres) == TRUE) { 'NA' } else { tmp.result$adres }
      kvk                   = if (is.null(tmp.result$kvk) == TRUE) { 'NA' } else { tmp.result$kvk }
      handelsnamen.bestaand1= if (is.null(tmp.result$handelsnamen.bestaand1) == TRUE) { 'NA' } else { tmp.result$handelsnamen.bestaand1 }
      handelsnamen.bestaand2= if (is.null(tmp.result$handelsnamen.bestaand2) == TRUE) { 'NA' } else { tmp.result$handelsnamen.bestaand2 }
      postcode              = if (is.null(tmp.result$postcode) == TRUE) { 'NA' } else { tmp.result$postcode }
      type                  = if (is.null(tmp.result$type) == TRUE) { 'NA' } else { tmp.result$type }
      kvks                  = if (is.null(tmp.result$kvks) == TRUE) { 'NA' } else { tmp.result$kvks }
      woonplaats            = if (is.null(tmp.result$woonplaats) == TRUE) { 'NA' } else { tmp.result$woonplaats }
      
      tmp.subresult = data.frame(
        rechtspersoon = rechtspersoon,
        vestigingsnummer = vestigingsnummer,
        adres = adres,
        kvk = kvk,
        handelsnamen.bestaand1 = handelsnamen.bestaand1,
        handelsnamen.bestaand2 = handelsnamen.bestaand2,
        postcode = postcode,
        type = type,
        kvks = kvks,
        woonplaats = woonplaats, 
        stringsAsFactors=FALSE,
        check.names=TRUE)

      
      
      if ( i == 1 ) {
        # first record in set
        final.resultset = tmp.subresult
      } else {
        # add row to dataset
        final.resultset = rbind(final.resultset, tmp.subresult)
      }
  
    } else {
      print('no results')
      
    }
  }
  # output result
  final.resultset  
}



x = getCompanies(filename='data/input_companies_longer.txt')

