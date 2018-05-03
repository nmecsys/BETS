# examples extraction
library(BETS)

vec <- c(24369, 13522, 4466, 22109, 433, 7326, 10810,
         11777, 1208, 4447, 14, 21864, 4189, 432, 4192, 1208)

examples <- data.frame(date = character(), value = numeric(), code = integer())
metadata_pt <- data.frame(code = character(), description = character(), unit = character(), periodicity = character(), start = character(), last_value = character(), source = character())
metadata_en <- metadata_pt

for(v in vec){
    examples <- rbind(examples, cbind(BETSget(v, data.frame = T),v))
    metadata_pt <- rbind(metadata_pt, BETSsearch(code = v, lang = "pt", view = F))
    metadata_en <- rbind(metadata_en, BETSsearch(code = v, lang = "en", view = F))
}

saveRDS(examples)
saveRDS(metadata_pt)
saveRDS(metadata_en)


