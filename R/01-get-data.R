
# Obtaining Lattes data ---------------------------------------------------

dir <- list.dirs("./Data")[-1]
folders <- list.files("./Data")
cv <- list()

n <- as.numeric()
for (m in 1:length(dir)) {
  n[m] <- length(list.files(dir[m]))
  num.row <- sum(n)
}; rm(n)

c.names <- c("universidade", "pais.origem", "bsc.inicio.ano", "bsc.fim.ano", "msc.inicio.year", "msc.fim.year", "phd.inicio.year", 
             "phd.fim.year", "n.publicacao", "n.aceitos", "n.supervisoes", "n.conferencias", "n.artigos.ingles", "id","ano.inicial",
             "max.IF")
num.col <- length(c.names)

df <- as.data.frame(matrix(ncol = num.col, nrow = num.row))
colnames(df) <- c.names

for (i in 1:length(dir)) {
  files <- list.files(dir[i])
  if (i == 1){
    for (j in 1:length(files)) {
      end <- paste(dir[i],files[j], sep = "/")
      cv[[j]] <- gld_get_lattes_data_from_zip(end)
      df[j,1] <- folders[i]
      df[j,2] <- ifelse(is.null(cv[[j]][["tpesq"]][["country.origin"]]), NA, cv[[j]][["tpesq"]][["country.origin"]])
      df[j,3] <- ifelse(is.null(cv[[j]][["tpesq"]][["bsc.start.year"]]), NA, cv[[j]][["tpesq"]][["bsc.start.year"]])
      df[j,4] <- ifelse(is.null(cv[[j]][["tpesq"]][["bsc.end.year"]]), NA, cv[[j]][["tpesq"]][["bsc.end.year"]])
      df[j,5] <- ifelse(is.null(cv[[j]][["tpesq"]][["msc.start.year"]]), NA, cv[[j]][["tpesq"]][["msc.start.year"]])
      df[j,6] <- ifelse(is.null(cv[[j]][["tpesq"]][["msc.end.year"]]), NA, cv[[j]][["tpesq"]][["msc.end.year"]])
      df[j,7] <- ifelse(is.null(cv[[j]][["tpesq"]][["phd.start.year"]]), NA, cv[[j]][["tpesq"]][["phd.start.year"]])
      df[j,8] <- ifelse(is.null(cv[[j]][["tpesq"]][["phd.end.year"]]), NA, cv[[j]][["tpesq"]][["phd.end.year"]])
      df[j,9] <- ifelse(dim(cv[[j]][["tpublic.published"]])[1] == 0, NA, dim(cv[[j]][["tpublic.published"]][1]))
      df[j,10] <- ifelse(dim(cv[[j]][["tpublic.accepted"]])[1] == 0, NA, dim(cv[[j]][["tpublic.accepted"]][1]))
      df[j,11] <- ifelse(is.null(dim(cv[[j]][["tsupervisions"]])[1]), NA, dim(cv[[j]][["tsupervisions"]])[1])
      df[j,12] <- ifelse(is.null(dim(cv[[j]][["tconferences"]])[1]), NA, dim(cv[[j]][["tconferences"]])[1])
      df[j,13] <- ifelse(is.null(table(cv[[j]][["tpublic.published"]][["language"]])[1]), NA, sum(cv[[j]][["tpublic.published"]][["language"]] == "Inglês"))
      df[j,14] <- cv[[j]][["tpesq"]][["id.file"]]
      df[j,15] <- ifelse(is.null(cv[[j]][["tpublic.published"]][["year"]]), NA, min(cv[[j]][["tpublic.published"]][["year"]]))
      df[j,16] <- ifelse(is.null(cv[[j]][["tpublic.published"]][["SJR"]]), NA, max(na.omit(cv[[j]][["tpublic.published"]][["SJR"]])))
    }
  }
  if (i != 1) {
    for (k in 2:i) {
      n <- ifelse(k == 2, length(list.files(dir[1])), sum(length(list.files(dir[k-1])), n))
    }
    for (j in 1:length(files)) {
      end <- paste(dir[i],files[j], sep = "/")
      cv[[n+j]] <- gld_get_lattes_data_from_zip(end)
      df[n+j,1] <- folders[i]
      df[n+j,2] <- ifelse(is.null(cv[[n+j]][["tpesq"]][["country.origin"]]), NA, cv[[n+j]][["tpesq"]][["country.origin"]])
      df[n+j,3] <- ifelse(is.null(cv[[n+j]][["tpesq"]][["bsc.start.year"]]), NA, cv[[n+j]][["tpesq"]][["bsc.start.year"]])
      df[n+j,4] <- ifelse(is.null(cv[[n+j]][["tpesq"]][["bsc.end.year"]]), NA, cv[[n+j]][["tpesq"]][["bsc.end.year"]])
      df[n+j,5] <- ifelse(is.null(cv[[n+j]][["tpesq"]][["msc.start.year"]]), NA, cv[[n+j]][["tpesq"]][["msc.start.year"]])
      df[n+j,6] <- ifelse(is.null(cv[[n+j]][["tpesq"]][["msc.end.year"]]), NA, cv[[n+j]][["tpesq"]][["msc.end.year"]])
      df[n+j,7] <- ifelse(is.null(cv[[n+j]][["tpesq"]][["phd.start.year"]]), NA, cv[[n+j]][["tpesq"]][["phd.start.year"]])
      df[n+j,8] <- ifelse(is.null(cv[[n+j]][["tpesq"]][["phd.end.year"]]), NA, cv[[n+j]][["tpesq"]][["phd.end.year"]])
      df[n+j,9] <- ifelse(dim(cv[[n+j]][["tpublic.published"]])[1] == 0, NA, dim(cv[[n+j]][["tpublic.published"]][1]))
      df[n+j,10] <- ifelse(dim(cv[[n+j]][["tpublic.accepted"]])[1] == 0, NA, dim(cv[[n+j]][["tpublic.accepted"]][1]))
      df[n+j,11] <- ifelse(is.null(dim(cv[[n+j]][["tsupervisions"]])[1]), NA, dim(cv[[n+j]][["tsupervisions"]])[1])
      df[n+j,12] <- ifelse(is.null(dim(cv[[n+j]][["tconferences"]])[1]), NA, dim(cv[[n+j]][["tconferences"]])[1])
      df[n+j,13] <- ifelse(is.null(table(cv[[n+j]][["tpublic.published"]][["language"]])[1]), NA, sum(cv[[n+j]][["tpublic.published"]][["language"]] == "Inglês"))
      df[n+j,14] <- cv[[n+j]][["tpesq"]][["id.file"]]
      df[n+j,15] <- ifelse(is.null(cv[[n+j]][["tpublic.published"]][["year"]]), NA, min(cv[[n+j]][["tpublic.published"]][["year"]]))
      df[n+j,16] <- ifelse(is.null(cv[[n+j]][["tpublic.published"]][["SJR"]]), NA, max(na.omit(cv[[n+j]][["tpublic.published"]][["SJR"]])))
    }
  }
  message(paste("\n\nDONE", i, "of", length(dir), "folders"))
}; rm(c.names,dir,end,files,folders,i,j,k,m,n,num.col,num.row,cv)


# Calculating academic productivity index ---------------------------------

# Function to standardize from -1 to 1

minmax_neg <- function(x){
  return((x - ((max(x) + min(x)) / 2)) / ((max(x) - min(x)) / 2))
}

df <- df %>% mutate(prod.rate = n.publicacao/(2022-ano.inicial))
df <- df %>% mutate(index = prod.rate*max.IF)
df <- df %>% mutate(standard.index = minmax_neg(index))

# creating a new directory to save data

if (file.exists("./Data_clean")) {
  cat("The folder already exists")
} else {
  dir.create("./Data_clean")
}

# Salving Lattes data

write.xlsx(x = df, 
           file = "./Data_clean/Lattes_data.xlsx", 
           sheetName = "Lattes",
           col.names = TRUE,
           row.names = FALSE)

PPGs <- df %>% 
  group_by(universidade) %>% 
  summarise(media = mean(standard.index),
            mediana = median(standard.index),
            maximo = max(standard.index),
            minimo = min(standard.index),
            N = length(universidade))

PPGs <- PPGs %>% mutate(N_normalizado = (N-min(N))/(max(N)-min(N)))

# saving productivity index by PPGs

write.xlsx(x = PPGs, 
           file = "./Data_clean/PPGs.xlsx", 
           sheetName = "Productivity",
           col.names = TRUE,
           row.names = T)
