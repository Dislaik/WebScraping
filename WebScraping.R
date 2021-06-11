library(rvest)
library(stringr)
library(xlsx)

pagesURL <- str_c("https://listado.mercadolibre.cl/computacion/notebooks/_Desde_", seq(1, 100, by = 50))

DataProducts <- c()
DataProducts.URL <- c()
DataProducts.Title <- c()
DataProducts.Model <- c()
DataProducts.Price <- c()
DataProducts.Sold <- c()
DataProducts.Location <- c()
DataProducts.TotalSales <- c()

for (URL in pagesURL)  {
    pageHTML <- read_html(URL)
  
    pagination <- html_nodes(pageHTML, ".ui-search-result__content-wrapper")
    productLink <- html_attr(html_nodes(pagination, ".ui-search-item__group__element.ui-search-link"), "href")
  
    for (URLProduct in productLink) {
        productPageHTML <- read_html(URLProduct)
        
        product <- html_nodes(productPageHTML, ".ui-pdp-container__col.col-1.ui-pdp-container--column-right.mt-16.pr-16")
        
        DataProducts.URL <- c(DataProducts.URL, URLProduct)
        print(URLProduct)
        
        productTitle <- html_nodes(product, ".ui-pdp-title") #Product Title
        productTitle <- html_text(productTitle)
        DataProducts.Title <- c(DataProducts.Title, productTitle)
        
        productModel <- html_nodes(productPageHTML, ".andes-breadcrumb__link") #Product Model
        productModel <- html_text(productModel)
        
        if (length(productModel[4]) == 1 & !is.na(productModel[4])) {
          DataProducts.Model <- c(DataProducts.Model, productModel[4])
        } else {
          DataProducts.Model <- c(DataProducts.Model, "Desconocido")
        }
        
        productPriceContainer <- html_nodes(product, ".ui-pdp-price__second-line") #Product Price Container
        productPrice <- html_nodes(productPriceContainer, ".price-tag-fraction") #Product Price
        productPrice <- html_text(productPrice)
        productPrice <- as.numeric(gsub("\\.", "", productPrice))
        DataProducts.Price <- c(DataProducts.Price, productPrice)
        
        productSold <- html_nodes(product, ".ui-pdp-subtitle") #Product Sold
        productSold <- html_text(productSold)
        productSold <- unlist(strsplit(productSold, "\\ "))
        
        found <- FALSE
        for (i in productSold) {

          if (grepl("^[[:digit:]]", i) == TRUE) {
            productSold <- as.numeric(i)
            
            found <- TRUE
            break
          }
          
        }
        if (!found) {
          productSold <- 0
        }
        
        DataProducts.Sold <- c(DataProducts.Sold, productSold)
        
        productLocation <- html_nodes(product, ".ui-seller-info__status-info__subtitle") #Product Location
        productLocation <- html_text(productLocation)
        print(productLocation)
        
        if (productLocation[1] == "¡Es uno de los mejores del sitio!" | length(productLocation) == 0) {
          DataProducts.Location <- c(DataProducts.Location, "Desconocido")
        } else {
          DataProducts.Location <- c(DataProducts.Location, productLocation[1])
        }
        
        productTotalSales <- html_nodes(product, ".ui-pdp-seller__sales-description") #Product Total Sales 
        productTotalSales <- html_text(productTotalSales)
        productTotalSales <- as.numeric(productTotalSales)
        
        if (length(productTotalSales) == 1) {
          DataProducts.TotalSales <- c(DataProducts.TotalSales, productTotalSales)
        } else {
          DataProducts.TotalSales <- c(DataProducts.TotalSales, 0)
        }
        
    }
}

productos <- data.frame(
  url = DataProducts.URL,
  titulo = DataProducts.Title,
  modelo = DataProducts.Model,
  precio = DataProducts.Price,
  vendidos = DataProducts.Sold,
  ubicacion = DataProducts.Location,
  total_ventas = DataProducts.TotalSales
)

write.csv(productos, file="MercadoLibreDatos.csv", row.names = FALSE)
