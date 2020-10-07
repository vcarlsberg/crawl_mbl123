library(Rcrawler)
library("selectr")
library("xml2")
library("rvest")
library("stringr")
library("httr")
library("xlsx")

mobil<-data.frame(matrix(ncol = 11, nrow = 0))
colnames(mobil) <- c("harga","tahun","mileage","variant","transmisi","warna","mesin","merk","model","penumpang","lokasi")
a=0

for (page_number in 1:70){
  url<-
    paste("https://www.mobil123.com/mobil-dijual/indonesia?type=used&profile_type=Private&page_number=",page_number,"&page_size=50",sep="")
  webpage <- read_html(url)
  
  
  
  for (line in 1:110){
    link<- tryCatch(
      {
        text<-
          xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(
            webpage, 2), 3), 6), 1), 2), 2), 1), 1),as.integer(line))
        xml_attrs(text)[["data-url"]]
      },
      error = function(e){
        #      message('Caught an error!')
        #      print(e)
      }
    )
    
    
    if (is.null(link) == FALSE)
    {
      a<-a+1
      #print(paste(a,link))
      
      webpage_product_detail <- read_html(link)
      
      product_detail<-html_text(html_nodes(webpage_product_detail, ".listing__key-listing__list"))
      product_detail<-str_replace_all(product_detail, "[\r\n]" , "")
      product_detail<-str_replace_all(product_detail, "\\s+", " ")
      
      
      print(paste(a,product_detail))
      #hasil2<-html_text(html_nodes(webpage3, ".listing__key-listing__list"))
      
      #html_nodes(webpage3, ".listing__key-listing__list")
      
      #hasil3<-html_text(html_nodes(webpage3, ".listing__price.delta.weight--bold"))
      #hasil3<-html_text(html_nodes(webpage3, ".float--right"))
      
      #hasil3[4]
      detail_harga<-html_text(html_nodes(webpage_product_detail, ".listing__price.delta.weight--bold"))
      detail_others<-html_text(html_nodes(webpage_product_detail, ".float--right"))
      detail_location<-html_text(html_nodes(webpage_product_detail, "div.list-item.soft-half--ends"))
      
      detail_mboh<-html_text(html_nodes(webpage_product_detail, "list-item  soft-half  "))
      #icon  icon--calendar  icon--secondary  push-half--right
      
      harga_data_nonclean<-trimws(substr(detail_harga[1],as.data.frame(str_locate_all(pattern ="p", detail_harga[1]))$end+1,nchar(detail_harga[1])))
      harga_data<-as.numeric(str_replace_all(harga_data_nonclean, "\\." , ""))
      
      
      tahun_data<-as.numeric(trimws(substr(product_detail,as.data.frame(str_locate_all(pattern ="Tahun", product_detail))$end+1,as.data.frame(str_locate_all(pattern ="Cakupan", product_detail))$start-1)))
      mileage_data<-as.integer(xml_attrs(text)[["data-mileage"]])
      transmisi_data<-trimws(substr(product_detail,as.data.frame(str_locate_all(pattern ="Transmisi", product_detail))$end+1,as.data.frame(str_locate_all(pattern ="Penumpang", product_detail))$start-1))
      warna_data<-trimws(substr(product_detail,as.data.frame(str_locate_all(pattern ="Warna", product_detail))$end+1,nchar(product_detail)))
      
      # variant_data<- tryCatch(
      #   {
      #     trimws(substr(product_detail,as.data.frame(str_locate_all(pattern ="Varian", product_detail))$end+1,as.data.frame(str_locate_all(pattern ="Tahun", product_detail))$start-1))
      #   },
      #   error = function(e){
      #     return("")
      #   }
      # )
      
      #variant_data<-trimws(substr(product_detail,as.data.frame(str_locate_all(pattern ="Varian", product_detail))$end+1,as.data.frame(str_locate_all(pattern ="Tahun", product_detail))$start-1))
      
      variant_model_data<-trimws(substr(product_detail,as.data.frame(str_locate_all(pattern ="Model", product_detail))$end+1,as.data.frame(str_locate_all(pattern ="Tahun", product_detail))$start-1))
      
      if (str_detect(variant_model_data, "Varian")==FALSE) {
        model_data<-variant_model_data
        variant_data<-""
      }else{
        lokasi_awal_varian<-as.data.frame(str_locate_all(pattern ="Varian", variant_model_data))$start
        lokasi_akhir_varian<-as.data.frame(str_locate_all(pattern ="Varian", variant_model_data))$end
        
        model_data<-trimws(substr(variant_model_data,1,lokasi_awal_varian-1))
        variant_data<-trimws(substr(variant_model_data,lokasi_akhir_varian+1,nchar(variant_model_data)))
      }
      
      mesin_data<-as.integer(trimws(substr(product_detail,as.data.frame(str_locate_all(pattern ="Cakupan mesin", product_detail))$end+1,as.data.frame(str_locate_all(pattern ="cc", product_detail))$start-1)))
      merk_data<-trimws(substr(product_detail,as.data.frame(str_locate_all(pattern ="Merek", product_detail))$end+1,as.data.frame(str_locate_all(pattern ="Model", product_detail))$start-1))
      #model_data<-trimws(substr(product_detail,as.data.frame(str_locate_all(pattern ="Model", product_detail))$end+1,as.data.frame(str_locate_all(pattern ="Varian", product_detail))$start-1))
      location_data<-str_replace_all(trimws(detail_location[2]), "\\s+", " ")
      penumpang_data<-as.integer(trimws(substr(product_detail,as.data.frame(str_locate_all(pattern ="Penumpang", product_detail))$end+1,as.data.frame(str_locate_all(pattern ="Kilometer", product_detail))$start-1)))
      
      #xp_varian<-"/html/body/main/div[2]/div/div/article/div[2]/div[2]/div[6]/div/div[4]/span[2]"
      #xp_varian<-webpage_product_detail %>% 
      #  html_nodes(xpath = xp_varian) %>% 
      #  html_text()
      
      mobil<-rbind(mobil,data.frame(harga=harga_data,
                                    tahun=tahun_data,
                                    mileage=mileage_data,
                                    variant=variant_data,
                                    transmisi=transmisi_data,
                                    warna=warna_data,
                                    mesin=mesin_data,
                                    merk=merk_data,
                                    model=strsplit(model_data," ")[[1]][1],
                                    penumpang=penumpang_data,
                                    lokasi=location_data
      )
      )
    }
    
    #invisible(readline(prompt="Press [enter] to continue"))
    
  }
}

#tryCatch(sqrt("a"), 
#         error=function(e) 
#           print("You can't take the square root of a character, silly!"))  

write.csv(mobil, "file.csv", 
          col.names=TRUE, row.names=TRUE, append=FALSE)

#replace \n di variabel product_detail
#str_replace_all(x, "[\r\n]" , "")

#ngambil parameter di prooduct_detail
#xxxy<-str_locate_all(pattern ="Merek", katakata)
#xxyy<-as.data.frame(str_locate_all(pattern ="Merek", katakata))
#substr(katakata, xxyy$start, xxyy$end)