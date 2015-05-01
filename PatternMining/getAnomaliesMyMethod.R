#!/usr/bin/env Rscript

# supporting_node_file'deki her satir, patterns_file'de ayni no'lu satira denk geliyor
# alfa ve beta 0 ile 1 arasinda olsun ki (sup_nodes*growth_rate) sonucu fazla cikmasin
# alfa + beta = 1 olsun
# alfa ve beta, growth_rate ve sup_nodes degerlerinin importance'ini veriyor
# mesela alfa=0.7 ve beta=0.3 olsun. Yani growth_rate*0.3 + sup_nodes*beta olur

# NOT: ALFA VE BETA COK DA SONUCU DEGISTIRMIYOR

# Return: None

############
# FUNCTION #
############

getAnomaliesMyMethod = function(patterns_file, supporting_node_file, db_file, output_file="output.csv",
                                alfa=1, beta=1){
  nodes = read.csv(supporting_node_file, header=FALSE, strip.white=TRUE, stringsAsFactors=FALSE)
  growth.rate = read.csv(file=patterns_file, header=TRUE, stringsAsFactors=FALSE)

  db = read.csv(db_file, header=FALSE, strip.white=TRUE, stringsAsFactors=FALSE)

  growth.rate$nodes = nodes['V1']
  comms = unique(growth.rate['comm'])
  threshold=0
  output.data = c()

  for(c in comms[,]){
    #cat("\nkomun:", c, "\n")
    # komun pattern'ini her satirda bulabilmek icin grep'e gore ceviriyoruz
    cc=gsub(" ", ".*", c)
    comm.nodes=c();
    for(i in seq(1,nrow(db))){
      if(grepl(cc, paste(db[i,],collapse=","))){
        comm.nodes=c(comm.nodes,i)
      }
    }
    # c komununde olan node id'leri bulmus olduk

    set = growth.rate[which(growth.rate['comm'] == c),]
    attrs=c()
    # bir komunde aciga cikan attribute'leri bi yerde topluyoruz
    for(a in set['attr'][,]){
      attrs = unique(c(attrs, strsplit(a, split=" ")[[1]]))
    }

    attr.types = unique(substring(attrs,1,1))

    #############################################################
    # 1 tane data.frame kullanicaz hesaplamalari stoklamak icin #
    #############################################################

    # data frame'i initialize ediyoruz
    df <- data.frame(matrix(0, nrow = length(attr.types), ncol = 5))
    names(df) = c("size", "growth.rate", "result", "attr", "sup.nodes")
    df[,"sup.nodes"] = rep("", nrow(df))

    att.names = c()

    count = 1
    for(attr.type in attr.types){
      #  'att' => ayni tip attribute'leri ayni listeye aliyoruz (s1 ve S2 gibi)
      att = attrs[which(substring(attrs,1,1) == attr.type)]
      att.name = paste(att, collapse="")
      # attribute isimlerini olusturuyoruz
      att.names = c(att.names, att.name)

      for(a in att){
        ind = which(grepl(a, set['pattern'][,]))
        gr = set[ind,]
        # a="S1" mesela, a'yi iceren bir suru pattern olabilir
        # bu yuzden B8S1T1 yerine B8S1'i secmek icin sup_pattern'de max olani aliriz
        chosen.pattern = gr[which.max(gr["sup_pattern"][,]),]
        col1 = as.numeric(chosen.pattern['sup_pattern']) + as.numeric(df[count,"size"])
        col2 = as.numeric(chosen.pattern['growth_rate']) + as.numeric(df[count,"growth.rate"])
        col3 = (alfa*col1)*(beta*col2) + as.numeric(df[count, "result"])
        col4 = paste(att, collapse=" ")
        previous.part = df[count, 'sup.nodes']
        part = chosen.pattern['nodes'][1,1]
        col5 = sub("^\\s+", "", paste(previous.part, part, sep=" "))
        df[count,] = c(col1, col2, col3, col4, col5)
      }
      count = count+1
    }
    row.names(df) = att.names
    #"Result" kolonuna gore buyukten kucuge siraladik
    # - isareti buyukten kucuge olmasini sagliyor
    df = df[order(-as.numeric(df[,"result"])),]


    ########################################################################
    # Listedeki her satir icin intersection'siz pattern nodelari ekliyoruz #
    ########################################################################

    next.set = NA
    final.results = data.frame(matrix(0, nrow = length(attr.types), ncol = 8))
    names(final.results) = c("result", "growth.rate", "comm", "comm.size", "sup.nodes.size", "sup.nodes", "attr", "anomaly")
    final.results[,c("sup.nodes", "anomaly", "attr")] = rep("", nrow(final.results))
    final = 0
    for(i in seq(1,length(attr.types))){
      final = final+1
      change = TRUE
      times = 0
      while(change){
        times = times + 1
        # data frame'i initialize ediyoruz
        possible.patterns <- data.frame(matrix(0, nrow = length(attr.types), ncol = 5))
        names(possible.patterns) = c("size", "growth.rate", "result", "attr", "sup.nodes")
        possible.patterns[,"sup.nodes"] = rep("", nrow(possible.patterns))
        count = 1
        change = FALSE

        covered = df[i,"sup.nodes"]
        covered.list = strsplit(covered, " ")[[1]]

        # i. satirdaki pattern'in icerdigi attribute'lere sahip olmayan satirlara bakiyoruz
        df.i.attr = strsplit(df[i,"attr"], " ")[[1]]
        ind = c()

        if(nrow(set) == 0){ break }

        #print("satir 107")
        for(b in seq(1,nrow(set))){
          if(any(strsplit(set[b,"attr"], " ")[[1]] %in% df.i.attr)){ ind = c(ind, b) }
        }
        #ilgili satirla ilgilenecegimiz icin bazi satirlari filtreliyoruz
        set.filtered = set[-ind,]

        #bir sonraki iterasyonda kullanilacak olan kumeyi belirliyoruz
        if(times == 1){
          next.set = set.filtered
        }

        #while'i bitirme kosulu
        if(nrow(set.filtered) < 1){ break }

        for(j in seq(1, nrow(set.filtered))){
          next.sup.nodes = as.character(set.filtered[j,"nodes"])
          next.sup.nodes.list = strsplit(next.sup.nodes," ")[[1]]
          matched.indexes = which(covered.list %in% next.sup.nodes.list)
          if(threshold >= length(matched.indexes)){
            change = TRUE
            col1 = as.numeric(set.filtered[j,"sup_pattern"])
            col2 = as.numeric(set.filtered[j,"growth_rate"])
            col4 = set.filtered[j,"attr"]
            possible.patterns[count,] = c(col1, col2, (alfa*col1)*(beta*col2), col4, next.sup.nodes)
            count = count + 1
          }
        }

        # Eger herhangi bi sup.nodes eklemesi basarabildiysek
        if(change){
          best.choice = possible.patterns[which.max(possible.patterns[,"result"]),]
          col1 = as.numeric(df[i,"size"]) + as.numeric(best.choice[1,"size"])
          col2 = as.numeric(df[i,"growth.rate"]) + as.numeric(best.choice[1,"growth.rate"])
          col4 = paste(df[i,"attr"], best.choice[1,"attr"], sep=" ")
          mixte = c(covered.list, strsplit(best.choice[1,"sup.nodes"], " ")[[1]])
          col5 = paste(mixte[order(mixte)], collapse=" ")
          df[i,] = c(col1, col2, (alfa*col1)*(beta*col2), col4, col5)
        }
      }

      # i. satirdaki pattern'in butun olasiliklarini gormus olduk
      # en iyi secim: df[i,]
      # ve comm.nodes ile karsilastirip anomali nodelari bulucaz
      in.indexes = which(comm.nodes %in% strsplit(df[i,"sup.nodes"]," ")[[1]])
      anomaly.nodes = paste(comm.nodes[-c(in.indexes)], collapse=" ")
      all.attr = df[i,"attr"]
      supporting.nodes = df[i, "sup.nodes"]
      current.total.sup.nodes = length(strsplit(supporting.nodes, " ")[[1]])
      growth.r = df[i,"growth.rate"]
      result = as.numeric(df[i,"size"]) * as.numeric(growth.r)
      comm = c
      comm.size = length(comm.nodes)
      output = c(result, growth.r, c, comm.size, current.total.sup.nodes, supporting.nodes, all.attr, anomaly.nodes)
      final.results[final,] = output
      #print(output)

      #set degisimini yapiyoruz
      set = next.set
    }
    #print(final.results)
    output.data = rbind(output.data, final.results[which.max(final.results[,"result"]),])
  }

  names(output.data) = c("result", "growth.rate", "comm", "comm.size", "sup.nodes.size", "sup.nodes", "attr", "anomaly")
  write.csv(output.data, file=output_file, row.names=FALSE)
} # end function


##################
# INPUT AND TEST #
##################

##### TEST 1 #####
patterns_file = "growth_rate1.csv"
supporting_node_file = "supporting_nodes1.csv"
db_file = "walktrap_weighted_community.csv"

getAnomaliesMyMethod(patterns_file, supporting_node_file, db_file, "output1.csv", alfa=1, beta=1)

##### TEST 2 #####
patterns_file = "growth_rate2.csv"
supporting_node_file = "supporting_nodes2.csv"
db_file = "walktrap_weighted_community.csv"

getAnomaliesMyMethod(patterns_file, supporting_node_file, db_file, "output2.csv", alfa=1, beta=1)

##### TEST 3 #####
patterns_file = "growth_rate3.csv"
supporting_node_file = "supporting_nodes3.csv"
db_file = "walktrap_weighted_community.csv"

getAnomaliesMyMethod(patterns_file, supporting_node_file, db_file, "output3.csv", alfa=1, beta=1)
