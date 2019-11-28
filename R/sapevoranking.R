#' @title teste
#'
#' @description teste
#'
#' @param symbol
#'
#' @return NULL
#'
#' @examples
#'
#' @export

sapevo_ranking=function(projeto,decisores,alternativas,criterios,vetor_peso,
                        #---matriz avalia??o
                        #---criterio1
                        vetor_notas_decisor1criterio1=NULL,vetor_notas_decisor2criterio1=NULL,
                        vetor_notas_decisor3criterio1=NULL,vetor_notas_decisor4criterio1=NULL,
                        vetor_notas_decisor5criterio1=NULL,vetor_notas_decisor6criterio1=NULL,
                        vetor_notas_decisor7criterio1=NULL,vetor_notas_decisor8criterio1=NULL,
                        vetor_notas_decisor9criterio1=NULL,vetor_notas_decisor10criterio1=NULL,
                        #---criterio2
                        vetor_notas_decisor1criterio2=NULL,vetor_notas_decisor2criterio2=NULL,
                        vetor_notas_decisor3criterio2=NULL,vetor_notas_decisor4criterio2=NULL,
                        vetor_notas_decisor5criterio2=NULL,vetor_notas_decisor6criterio2=NULL,
                        vetor_notas_decisor7criterio2=NULL,vetor_notas_decisor8criterio2=NULL,
                        vetor_notas_decisor9criterio2=NULL,vetor_notas_decisor10criterio2=NULL,
                        #---criterio3
                        vetor_notas_decisor1criterio3=NULL,vetor_notas_decisor2criterio3=NULL,
                        vetor_notas_decisor3criterio3=NULL,vetor_notas_decisor4criterio3=NULL,
                        vetor_notas_decisor5criterio3=NULL,vetor_notas_decisor6criterio3=NULL,
                        vetor_notas_decisor7criterio3=NULL,vetor_notas_decisor8criterio3=NULL,
                        vetor_notas_decisor9criterio3=NULL,vetor_notas_decisor10criterio3=NULL,
                        #---criterio4
                        vetor_notas_decisor1criterio4=NULL,vetor_notas_decisor2criterio4=NULL,
                        vetor_notas_decisor3criterio4=NULL,vetor_notas_decisor4criterio4=NULL,
                        vetor_notas_decisor5criterio4=NULL,vetor_notas_decisor6criterio4=NULL,
                        vetor_notas_decisor7criterio4=NULL,vetor_notas_decisor8criterio4=NULL,
                        vetor_notas_decisor9criterio4=NULL,vetor_notas_decisor10criterio4=NULL,
                        #---criterio5
                        vetor_notas_decisor1criterio5=NULL,vetor_notas_decisor2criterio5=NULL,
                        vetor_notas_decisor3criterio5=NULL,vetor_notas_decisor4criterio5=NULL,
                        vetor_notas_decisor5criterio5=NULL,vetor_notas_decisor6criterio5=NULL,
                        vetor_notas_decisor7criterio5=NULL,vetor_notas_decisor8criterio5=NULL,
                        vetor_notas_decisor9criterio5=NULL,vetor_notas_decisor10criterio5=NULL,
                        #---criterio6
                        vetor_notas_decisor1criterio6=NULL,vetor_notas_decisor2criterio6=NULL,
                        vetor_notas_decisor3criterio6=NULL,vetor_notas_decisor4criterio6=NULL,
                        vetor_notas_decisor5criterio6=NULL,vetor_notas_decisor6criterio6=NULL,
                        vetor_notas_decisor7criterio6=NULL,vetor_notas_decisor8criterio6=NULL,
                        vetor_notas_decisor9criterio6=NULL,vetor_notas_decisor10criterio6=NULL,
                        #---criterio7
                        vetor_notas_decisor1criterio7=NULL,vetor_notas_decisor2criterio7=NULL,
                        vetor_notas_decisor3criterio7=NULL,vetor_notas_decisor4criterio7=NULL,
                        vetor_notas_decisor5criterio7=NULL,vetor_notas_decisor6criterio7=NULL,
                        vetor_notas_decisor7criterio7=NULL,vetor_notas_decisor8criterio7=NULL,
                        vetor_notas_decisor9criterio7=NULL,vetor_notas_decisor10criterio7=NULL,
                        #---criterio8
                        vetor_notas_decisor1criterio8=NULL,vetor_notas_decisor2criterio8=NULL,
                        vetor_notas_decisor3criterio8=NULL,vetor_notas_decisor4criterio8=NULL,
                        vetor_notas_decisor5criterio8=NULL,vetor_notas_decisor6criterio8=NULL,
                        vetor_notas_decisor7criterio8=NULL,vetor_notas_decisor8criterio8=NULL,
                        vetor_notas_decisor9criterio8=NULL,vetor_notas_decisor10criterio8=NULL,
                        #---criterio9
                        vetor_notas_decisor1criterio9=NULL,vetor_notas_decisor2criterio9=NULL,
                        vetor_notas_decisor3criterio9=NULL,vetor_notas_decisor4criterio9=NULL,
                        vetor_notas_decisor5criterio9=NULL,vetor_notas_decisor6criterio9=NULL,
                        vetor_notas_decisor7criterio9=NULL,vetor_notas_decisor8criterio9=NULL,
                        vetor_notas_decisor9criterio9=NULL,vetor_notas_decisor10criterio9=NULL,
                        #---criterio10
                        vetor_notas_decisor1criterio10=NULL,vetor_notas_decisor2criterio10=NULL,
                        vetor_notas_decisor3criterio10=NULL,vetor_notas_decisor4criterio10=NULL,
                        vetor_notas_decisor5criterio10=NULL,vetor_notas_decisor6criterio10=NULL,
                        vetor_notas_decisor7criterio10=NULL,vetor_notas_decisor8criterio10=NULL,
                        vetor_notas_decisor9criterio10=NULL,vetor_notas_decisor10criterio10=NULL

) {

  #matriz de avalia??o (independente do n?mero de alternativas)
  listas_notas_decisores_criterios1=list( vetor_notas_decisor1criterio1,
                                          vetor_notas_decisor2criterio1,
                                          vetor_notas_decisor3criterio1,
                                          vetor_notas_decisor4criterio1,
                                          vetor_notas_decisor5criterio1,
                                          vetor_notas_decisor6criterio1,
                                          vetor_notas_decisor7criterio1,
                                          vetor_notas_decisor8criterio1,
                                          vetor_notas_decisor9criterio1,
                                          vetor_notas_decisor10criterio1

  )

  listas_notas_decisores_criterios2=list( vetor_notas_decisor1criterio2,
                                          vetor_notas_decisor2criterio2,
                                          vetor_notas_decisor3criterio2,
                                          vetor_notas_decisor4criterio2,
                                          vetor_notas_decisor5criterio2,
                                          vetor_notas_decisor6criterio2,
                                          vetor_notas_decisor7criterio2,
                                          vetor_notas_decisor8criterio2,
                                          vetor_notas_decisor9criterio2,
                                          vetor_notas_decisor10criterio2




  )

  listas_notas_decisores_criterios3=list( vetor_notas_decisor1criterio3,
                                          vetor_notas_decisor2criterio3,
                                          vetor_notas_decisor3criterio3,
                                          vetor_notas_decisor4criterio3,
                                          vetor_notas_decisor5criterio3,
                                          vetor_notas_decisor6criterio3,
                                          vetor_notas_decisor7criterio3,
                                          vetor_notas_decisor8criterio3,
                                          vetor_notas_decisor9criterio3,
                                          vetor_notas_decisor10criterio3



  )

  listas_notas_decisores_criterios4=list( vetor_notas_decisor1criterio4,
                                          vetor_notas_decisor2criterio4,
                                          vetor_notas_decisor3criterio4,
                                          vetor_notas_decisor4criterio4,
                                          vetor_notas_decisor5criterio4,
                                          vetor_notas_decisor6criterio4,
                                          vetor_notas_decisor7criterio4,
                                          vetor_notas_decisor8criterio4,
                                          vetor_notas_decisor9criterio4,
                                          vetor_notas_decisor10criterio4



  )

  listas_notas_decisores_criterios5=list( vetor_notas_decisor1criterio5,
                                          vetor_notas_decisor2criterio5,
                                          vetor_notas_decisor3criterio5,
                                          vetor_notas_decisor4criterio5,
                                          vetor_notas_decisor5criterio5,
                                          vetor_notas_decisor6criterio5,
                                          vetor_notas_decisor7criterio5,
                                          vetor_notas_decisor8criterio5,
                                          vetor_notas_decisor9criterio5,
                                          vetor_notas_decisor10criterio5



  )

  listas_notas_decisores_criterios6=list( vetor_notas_decisor1criterio6,
                                          vetor_notas_decisor2criterio6,
                                          vetor_notas_decisor3criterio6,
                                          vetor_notas_decisor4criterio6,
                                          vetor_notas_decisor5criterio6,
                                          vetor_notas_decisor6criterio6,
                                          vetor_notas_decisor7criterio6,
                                          vetor_notas_decisor8criterio6,
                                          vetor_notas_decisor9criterio6,
                                          vetor_notas_decisor10criterio6



  )

  listas_notas_decisores_criterios7=list( vetor_notas_decisor1criterio7,
                                          vetor_notas_decisor2criterio7,
                                          vetor_notas_decisor3criterio7,
                                          vetor_notas_decisor4criterio7,
                                          vetor_notas_decisor5criterio7,
                                          vetor_notas_decisor6criterio7,
                                          vetor_notas_decisor7criterio7,
                                          vetor_notas_decisor8criterio7,
                                          vetor_notas_decisor9criterio7,
                                          vetor_notas_decisor10criterio7



  )

  listas_notas_decisores_criterios8=list( vetor_notas_decisor1criterio8,
                                          vetor_notas_decisor2criterio8,
                                          vetor_notas_decisor3criterio8,
                                          vetor_notas_decisor4criterio8,
                                          vetor_notas_decisor5criterio8,
                                          vetor_notas_decisor6criterio8,
                                          vetor_notas_decisor7criterio8,
                                          vetor_notas_decisor8criterio8,
                                          vetor_notas_decisor9criterio8,
                                          vetor_notas_decisor10criterio8



  )

  listas_notas_decisores_criterios9=list( vetor_notas_decisor1criterio9,
                                          vetor_notas_decisor2criterio9,
                                          vetor_notas_decisor3criterio9,
                                          vetor_notas_decisor4criterio9,
                                          vetor_notas_decisor5criterio9,
                                          vetor_notas_decisor6criterio9,
                                          vetor_notas_decisor7criterio9,
                                          vetor_notas_decisor8criterio9,
                                          vetor_notas_decisor9criterio9,
                                          vetor_notas_decisor10criterio9



  )

  listas_notas_decisores_criterios10=list( vetor_notas_decisor1criterio10,
                                           vetor_notas_decisor2criterio10,
                                           vetor_notas_decisor3criterio10,
                                           vetor_notas_decisor4criterio10,
                                           vetor_notas_decisor5criterio10,
                                           vetor_notas_decisor6criterio10,
                                           vetor_notas_decisor7criterio10,
                                           vetor_notas_decisor8criterio10,
                                           vetor_notas_decisor9criterio10,
                                           vetor_notas_decisor10criterio10



  )

  numero_alternativas=length(alternativas)
  num_criterios=length(criterios)
  total_criterio1=data.frame()
  for (i in 1:length(listas_notas_decisores_criterios1)) {

    if (is.null(listas_notas_decisores_criterios1[[i]])==F) {

      decisorpeso = matrix(listas_notas_decisores_criterios1[[i]],
                           nrow=numero_alternativas,
                           ncol=numero_alternativas,
                           byrow = TRUE)

      vetorpesonorm=rowSums(data.frame(decisorpeso))
      #print(vetorpesonorm1)
      vetorpesonorm=(vetorpesonorm-min(vetorpesonorm))/(max(vetorpesonorm)-min(vetorpesonorm))
      #print(vetorpesonorm1)
      total_criterio1=colSums(rbind(total_criterio1,vetorpesonorm))

    } else {

      cat("")

    }
  }

  total_criterio2=data.frame()
  for (i in 1:length(listas_notas_decisores_criterios2)) {

    if (is.null(listas_notas_decisores_criterios2[[i]])==F) {

      decisorpeso = matrix(listas_notas_decisores_criterios2[[i]],
                           nrow=numero_alternativas,
                           ncol=numero_alternativas,
                           byrow = TRUE)

      vetorpesonorm=rowSums(data.frame(decisorpeso))
      vetorpesonorm=(vetorpesonorm-min(vetorpesonorm))/(max(vetorpesonorm)-min(vetorpesonorm))
      total_criterio2=colSums(rbind(total_criterio2,vetorpesonorm))
    } else {

      cat("")

    }
  }

  total_criterio3=data.frame()
  for (i in 1:length(listas_notas_decisores_criterios3)) {

    if (is.null(listas_notas_decisores_criterios3[[i]])==F) {

      decisorpeso = matrix(listas_notas_decisores_criterios3[[i]],
                           nrow=numero_alternativas,
                           ncol=numero_alternativas,
                           byrow = TRUE)

      vetorpesonorm=rowSums(data.frame(decisorpeso))
      vetorpesonorm=(vetorpesonorm-min(vetorpesonorm))/(max(vetorpesonorm)-min(vetorpesonorm))
      total_criterio3=colSums(rbind(total_criterio3,vetorpesonorm))

    } else {

      cat("")

    }
  }


  total_criterio4=data.frame()
  for (i in 1:length(listas_notas_decisores_criterios4)) {

    if (is.null(listas_notas_decisores_criterios4[[i]])==F) {

      decisorpeso = matrix(listas_notas_decisores_criterios4[[i]],
                           nrow=numero_alternativas,
                           ncol=numero_alternativas,
                           byrow = TRUE)

      vetorpesonorm=rowSums(data.frame(decisorpeso))
      vetorpesonorm=(vetorpesonorm-min(vetorpesonorm))/(max(vetorpesonorm)-min(vetorpesonorm))
      total_criterio4=colSums(rbind(total_criterio4,vetorpesonorm))

    } else {

      cat("")

    }
  }

  total_criterio5=data.frame()
  for (i in 1:length(listas_notas_decisores_criterios5)) {

    if (is.null(listas_notas_decisores_criterios5[[i]])==F) {

      decisorpeso = matrix(listas_notas_decisores_criterios5[[i]],
                           nrow=numero_alternativas,
                           ncol=numero_alternativas,
                           byrow = TRUE)

      vetorpesonorm=rowSums(data.frame(decisorpeso))
      vetorpesonorm=(vetorpesonorm-min(vetorpesonorm))/(max(vetorpesonorm)-min(vetorpesonorm))
      total_criterio5=colSums(rbind(total_criterio5,vetorpesonorm))

    } else {

      cat("")

    }
  }

  total_criterio6=data.frame()
  for (i in 1:length(listas_notas_decisores_criterios6)) {

    if (is.null(listas_notas_decisores_criterios6[[i]])==F) {

      decisorpeso = matrix(listas_notas_decisores_criterios6[[i]],
                           nrow=numero_alternativas,
                           ncol=numero_alternativas,
                           byrow = TRUE)

      vetorpesonorm=rowSums(data.frame(decisorpeso))
      vetorpesonorm=(vetorpesonorm-min(vetorpesonorm))/(max(vetorpesonorm)-min(vetorpesonorm))
      total_criterio6=colSums(rbind(total_criterio6,vetorpesonorm))

    } else {

      cat("")

    }
  }

  total_criterio7=data.frame()
  for (i in 1:length(listas_notas_decisores_criterios7)) {

    if (is.null(listas_notas_decisores_criterios7[[i]])==F) {

      decisorpeso = matrix(listas_notas_decisores_criterios7[[i]],
                           nrow=numero_alternativas,
                           ncol=numero_alternativas,
                           byrow = TRUE)

      vetorpesonorm=rowSums(data.frame(decisorpeso))
      vetorpesonorm=(vetorpesonorm-min(vetorpesonorm))/(max(vetorpesonorm)-min(vetorpesonorm))
      total_criterio7=colSums(rbind(total_criterio7,vetorpesonorm))

    } else {

      cat("")

    }
  }

  total_criterio8=data.frame()
  for (i in 1:length(listas_notas_decisores_criterios8)) {

    if (is.null(listas_notas_decisores_criterios8[[i]])==F) {

      decisorpeso = matrix(listas_notas_decisores_criterios8[[i]],
                           nrow=numero_alternativas,
                           ncol=numero_alternativas,
                           byrow = TRUE)

      vetorpesonorm=rowSums(data.frame(decisorpeso))
      vetorpesonorm=(vetorpesonorm-min(vetorpesonorm))/(max(vetorpesonorm)-min(vetorpesonorm))
      total_criterio8=colSums(rbind(total_criterio8,vetorpesonorm))

    } else {

      cat("")

    }
  }

  total_criterio9=data.frame()
  for (i in 1:length(listas_notas_decisores_criterios9)) {

    if (is.null(listas_notas_decisores_criterios9[[i]])==F) {

      decisorpeso = matrix(listas_notas_decisores_criterios9[[i]],
                           nrow=numero_alternativas,
                           ncol=numero_alternativas,
                           byrow = TRUE)

      vetorpesonorm=rowSums(data.frame(decisorpeso))
      vetorpesonorm=(vetorpesonorm-min(vetorpesonorm))/(max(vetorpesonorm)-min(vetorpesonorm))
      total_criterio9=colSums(rbind(total_criterio9,vetorpesonorm))

    } else {

      cat("")

    }
  }

  total_criterios10=data.frame()
  for (i in 1:length(listas_notas_decisores_criterios10)) {

    if (is.null(listas_notas_decisores_criterios10[[i]])==F) {

      decisorpeso = matrix(listas_notas_decisores_criterios10[[i]],
                           nrow=numero_alternativas,
                           ncol=numero_alternativas,
                           byrow = TRUE)

      vetorpesonorm=rowSums(data.frame(decisorpeso))
      vetorpesonorm=(vetorpesonorm-min(vetorpesonorm))/(max(vetorpesonorm)-min(vetorpesonorm))
      total_criterio10=colSums(rbind(total_criterio10,vetorpesonorm))

    } else {

      cat("")

    }
  }


  if (num_criterios==1) {

    matriz_avaliacao=data.matrix(data.frame(total_criterio1))
    print(matriz_avaliacao)
    rownames(matriz_avaliacao)=alternativas

    colnames(matriz_avaliacao)=criterios

    vetor_ranking=matriz_avaliacao %*% vetor_peso

    names(vetor_ranking)=alternativas

    vetor_ranking_ordenado=cbind(sort(vetor_ranking,decreasing = T),1:length(alternativas))

    colnames(vetor_ranking_ordenado)=c("grau obtido","ranking")

    criterios_pontuacao=colSums(matriz_avaliacao)

    assign("matriz_avaliacao",matriz_avaliacao,envir = .GlobalEnv)
    assign("vetor_ranking_ordenado",vetor_ranking_ordenado,envir = .GlobalEnv)

    cat("---------O nome do seu projeto ?:",projeto,"---------","\n")
    cat("-------- Aternativas do seu projeto:",alternativas,"---------","\n")
    cat("---------Crit?rios do seu projeto:",criterios,"---------","\n")
    cat("---------Os pesos do m?todo sapevo-m s?o:",vetor_peso,"---------","\n\n")
    cat("---------A matriz avalia??o do seu projeto ?:","---------","\n")
    print(matriz_avaliacao)
    cat("\n")
    cat("---------A pontua??o dos crit?rios foi:","---------","\n")
    print(criterios_pontuacao)
    cat("\n")
    cat("---------O ranking do seu projeto ?:","---------","\n")
    print(vetor_ranking_ordenado)

  } else if (num_criterios==2)  {

    matriz_avaliacao=data.matrix(data.frame(total_criterio1,total_criterio2))

    rownames(matriz_avaliacao)=alternativas

    colnames(matriz_avaliacao)=criterios

    vetor_ranking=matriz_avaliacao %*% vetor_peso

    names(vetor_ranking)=alternativas

    vetor_ranking_ordenado=cbind(sort(vetor_ranking,decreasing = T),1:length(alternativas))

    colnames(vetor_ranking_ordenado)=c("grau obtido","ranking")

    criterios_pontuacao=colSums(matriz_avaliacao)

    assign("matriz_avaliacao",matriz_avaliacao,envir = .GlobalEnv)
    assign("vetor_ranking_ordenado",vetor_ranking_ordenado,envir = .GlobalEnv)

    cat("---------O nome do seu projeto ?:",projeto,"---------","\n")
    cat("-------- Aternativas do seu projeto:",alternativas,"---------","\n")
    cat("---------Crit?rios do seu projeto:",criterios,"---------","\n")
    cat("---------Os pesos do m?todo sapevo-m s?o:",vetor_peso,"---------","\n\n")
    cat("---------A matriz avalia??o do seu projeto ?:","---------","\n")
    print(matriz_avaliacao)
    cat("\n")
    cat("---------A pontua??o dos crit?rios foi:","---------","\n")
    print(criterios_pontuacao)
    cat("\n")
    cat("---------O ranking do seu projeto ?:","---------","\n")
    print(vetor_ranking_ordenado)

  } else if (num_criterios==3)  {

    matriz_avaliacao=data.matrix(data.frame(total_criterio1,total_criterio2,total_criterio3))

    rownames(matriz_avaliacao)=alternativas

    colnames(matriz_avaliacao)=criterios

    vetor_ranking=matriz_avaliacao %*% vetor_peso

    names(vetor_ranking)=alternativas

    vetor_ranking_ordenado=cbind(sort(vetor_ranking,decreasing = T),1:length(alternativas))

    colnames(vetor_ranking_ordenado)=c("grau obtido","ranking")

    criterios_pontuacao=colSums(matriz_avaliacao)

    assign("matriz_avaliacao",matriz_avaliacao,envir = .GlobalEnv)
    assign("vetor_ranking_ordenado",vetor_ranking_ordenado,envir = .GlobalEnv)

    cat("---------O nome do seu projeto ?:",projeto,"---------","\n")
    cat("-------- Aternativas do seu projeto:",alternativas,"---------","\n")
    cat("---------Crit?rios do seu projeto:",criterios,"---------","\n")
    cat("---------Os pesos do m?todo sapevo-m s?o:",vetor_peso,"---------","\n\n")
    cat("---------A matriz avalia??o do seu projeto ?:","---------","\n")
    print(matriz_avaliacao)
    cat("\n")
    cat("---------A pontua??o dos crit?rios foi:","---------","\n")
    print(criterios_pontuacao)
    cat("\n")
    cat("---------O ranking do seu projeto ?:","---------","\n")
    print(vetor_ranking_ordenado)

  } else if (num_criterios==4)  {

    matriz_avaliacao=data.matrix(data.frame(total_criterio1,total_criterio2,total_criterio3,total_criterio4))

    rownames(matriz_avaliacao)=alternativas

    colnames(matriz_avaliacao)=criterios

    vetor_ranking=matriz_avaliacao %*% vetor_peso

    names(vetor_ranking)=alternativas

    vetor_ranking_ordenado=cbind(sort(vetor_ranking,decreasing = T),1:length(alternativas))

    colnames(vetor_ranking_ordenado)=c("grau obtido","ranking")

    criterios_pontuacao=colSums(matriz_avaliacao)

    assign("matriz_avaliacao",matriz_avaliacao,envir = .GlobalEnv)
    assign("vetor_ranking_ordenado",vetor_ranking_ordenado,envir = .GlobalEnv)

    cat("---------O nome do seu projeto ?:",projeto,"---------","\n")
    cat("-------- Aternativas do seu projeto:",alternativas,"---------","\n")
    cat("---------Crit?rios do seu projeto:",criterios,"---------","\n")
    cat("---------Os pesos do m?todo sapevo-m s?o:",vetor_peso,"---------","\n\n")
    cat("---------A matriz avalia??o do seu projeto ?:","---------","\n")
    print(matriz_avaliacao)
    cat("\n")
    cat("---------A pontua??o dos crit?rios foi:","---------","\n")
    print(criterios_pontuacao)
    cat("\n")
    cat("---------O ranking do seu projeto ?:","---------","\n")
    print(vetor_ranking_ordenado)
  } else if (num_criterios==5)  {

    matriz_avaliacao=data.matrix(data.frame(total_criterio1,total_criterio2,total_criterio3,total_criterio4,total_criterio5))

    rownames(matriz_avaliacao)=alternativas

    colnames(matriz_avaliacao)=criterios

    vetor_ranking=matriz_avaliacao %*% vetor_peso

    names(vetor_ranking)=alternativas

    vetor_ranking_ordenado=cbind(sort(vetor_ranking,decreasing = T),1:length(alternativas))

    colnames(vetor_ranking_ordenado)=c("grau obtido","ranking")

    criterios_pontuacao=colSums(matriz_avaliacao)

    assign("matriz_avaliacao",matriz_avaliacao,envir = .GlobalEnv)
    assign("vetor_ranking_ordenado",vetor_ranking_ordenado,envir = .GlobalEnv)

    cat("---------O nome do seu projeto ?:",projeto,"---------","\n")
    cat("-------- Aternativas do seu projeto:",alternativas,"---------","\n")
    cat("---------Crit?rios do seu projeto:",criterios,"---------","\n")
    cat("---------Os pesos do m?todo sapevo-m s?o:",vetor_peso,"---------","\n\n")
    cat("---------A matriz avalia??o do seu projeto ?:","---------","\n")
    print(matriz_avaliacao)
    cat("\n")
    cat("---------A pontua??o dos crit?rios foi:","---------","\n")
    print(criterios_pontuacao)
    cat("\n")
    cat("---------O ranking do seu projeto ?:","---------","\n")
    print(vetor_ranking_ordenado)

  } else if (num_criterios==6)  {

    matriz_avaliacao=data.matrix(data.frame(total_criterio1,total_criterio2,total_criterio3,total_criterio4,total_criterio5,
                                            total_criterio6))

    rownames(matriz_avaliacao)=alternativas

    colnames(matriz_avaliacao)=criterios

    vetor_ranking=matriz_avaliacao %*% vetor_peso

    names(vetor_ranking)=alternativas

    vetor_ranking_ordenado=cbind(sort(vetor_ranking,decreasing = T),1:length(alternativas))

    colnames(vetor_ranking_ordenado)=c("grau obtido","ranking")

    criterios_pontuacao=colSums(matriz_avaliacao)

    assign("matriz_avaliacao",matriz_avaliacao,envir = .GlobalEnv)
    assign("vetor_ranking_ordenado",vetor_ranking_ordenado,envir = .GlobalEnv)

    cat("---------O nome do seu projeto ?:",projeto,"---------","\n")
    cat("-------- Aternativas do seu projeto:",alternativas,"---------","\n")
    cat("---------Crit?rios do seu projeto:",criterios,"---------","\n")
    cat("---------Os pesos do m?todo sapevo-m s?o:",vetor_peso,"---------","\n\n")
    cat("---------A matriz avalia??o do seu projeto ?:","---------","\n")
    print(matriz_avaliacao)
    cat("\n")
    cat("---------A pontua??o dos crit?rios foi:","---------","\n")
    print(criterios_pontuacao)
    cat("\n")
    cat("---------O ranking do seu projeto ?:","---------","\n")
    print(vetor_ranking_ordenado)

  } else if (num_criterios==7)  {

    matriz_avaliacao=data.matrix(data.frame(total_criterio1,total_criterio2,total_criterio3,total_criterio4,total_criterio5,
                                            total_criterio6,total_criterio7))

    rownames(matriz_avaliacao)=alternativas

    colnames(matriz_avaliacao)=criterios

    vetor_ranking=matriz_avaliacao %*% vetor_peso

    names(vetor_ranking)=alternativas

    vetor_ranking_ordenado=cbind(sort(vetor_ranking,decreasing = T),1:length(alternativas))

    colnames(vetor_ranking_ordenado)=c("grau obtido","ranking")

    criterios_pontuacao=colSums(matriz_avaliacao)

    assign("matriz_avaliacao",matriz_avaliacao,envir = .GlobalEnv)
    assign("vetor_ranking_ordenado",vetor_ranking_ordenado,envir = .GlobalEnv)

    cat("---------O nome do seu projeto ?:",projeto,"---------","\n")
    cat("-------- Aternativas do seu projeto:",alternativas,"---------","\n")
    cat("---------Crit?rios do seu projeto:",criterios,"---------","\n")
    cat("---------Os pesos do m?todo sapevo-m s?o:",vetor_peso,"---------","\n\n")
    cat("---------A matriz avalia??o do seu projeto ?:","---------","\n")
    print(matriz_avaliacao)
    cat("\n")
    cat("---------A pontua??o dos crit?rios foi:","---------","\n")
    print(criterios_pontuacao)
    cat("\n")
    cat("---------O ranking do seu projeto ?:","---------","\n")
    print(vetor_ranking_ordenado)

  } else if (num_criterios==8)  {

    matriz_avaliacao=data.matrix(data.frame(total_criterio1,total_criterio2,total_criterio3,total_criterio4,total_criterio5,
                                            total_criterio6,total_criterio7,total_criterio8))

    rownames(matriz_avaliacao)=alternativas

    colnames(matriz_avaliacao)=criterios

    vetor_ranking=matriz_avaliacao %*% vetor_peso

    names(vetor_ranking)=alternativas

    vetor_ranking_ordenado=cbind(sort(vetor_ranking,decreasing = T),1:length(alternativas))

    colnames(vetor_ranking_ordenado)=c("grau obtido","ranking")

    criterios_pontuacao=colSums(matriz_avaliacao)

    assign("matriz_avaliacao",matriz_avaliacao,envir = .GlobalEnv)
    assign("vetor_ranking_ordenado",vetor_ranking_ordenado,envir = .GlobalEnv)

    cat("---------O nome do seu projeto ?:",projeto,"---------","\n")
    cat("-------- Aternativas do seu projeto:",alternativas,"---------","\n")
    cat("---------Crit?rios do seu projeto:",criterios,"---------","\n")
    cat("---------Os pesos do m?todo sapevo-m s?o:",vetor_peso,"---------","\n\n")
    cat("---------A matriz avalia??o do seu projeto ?:","---------","\n")
    print(matriz_avaliacao)
    cat("\n")
    cat("---------A pontua??o dos crit?rios foi:","---------","\n")
    print(criterios_pontuacao)
    cat("\n")
    cat("---------O ranking do seu projeto ?:","---------","\n")
    print(vetor_ranking_ordenado)
  } else if (num_criterios==9)  {

    matriz_avaliacao=data.matrix(data.frame(total_criterio1,total_criterio2,total_criterio3,total_criterio4,total_criterio5,
                                            total_criterio6,total_criterio7,total_criterio8,total_criterio9))

    rownames(matriz_avaliacao)=alternativas

    colnames(matriz_avaliacao)=criterios

    vetor_ranking=matriz_avaliacao %*% vetor_peso

    names(vetor_ranking)=alternativas

    vetor_ranking_ordenado=cbind(sort(vetor_ranking,decreasing = T),1:length(alternativas))

    colnames(vetor_ranking_ordenado)=c("grau obtido","ranking")

    criterios_pontuacao=colSums(matriz_avaliacao)

    assign("matriz_avaliacao",matriz_avaliacao,envir = .GlobalEnv)
    assign("vetor_ranking_ordenado",vetor_ranking_ordenado,envir = .GlobalEnv)

    cat("---------O nome do seu projeto ?:",projeto,"---------","\n")
    cat("-------- Aternativas do seu projeto:",alternativas,"---------","\n")
    cat("---------Crit?rios do seu projeto:",criterios,"---------","\n")
    cat("---------Os pesos do m?todo sapevo-m s?o:",vetor_peso,"---------","\n\n")
    cat("---------A matriz avalia??o do seu projeto ?:","---------","\n")
    print(matriz_avaliacao)
    cat("\n")
    cat("---------A pontua??o dos crit?rios foi:","---------","\n")
    print(criterios_pontuacao)
    cat("\n")
    cat("---------O ranking do seu projeto ?:","---------","\n")
    print(vetor_ranking_ordenado)

  } else {
    matriz_avaliacao=data.matrix(data.frame(total_criterio1,total_criterio2,total_criterio3,total_criterio4,total_criterio5,
                                            total_criterio6,total_criterio7,total_criterio8,total_criterio9,total_criterio10))

    rownames(matriz_avaliacao)=alternativas

    colnames(matriz_avaliacao)=criterios

    vetor_ranking=matriz_avaliacao %*% vetor_peso

    names(vetor_ranking)=alternativas

    vetor_ranking_ordenado=cbind(sort(vetor_ranking,decreasing = T),1:length(alternativas))

    colnames(vetor_ranking_ordenado)=c("grau obtido","ranking")

    criterios_pontuacao=colSums(matriz_avaliacao)

    assign("matriz_avaliacao",matriz_avaliacao,envir = .GlobalEnv)
    assign("vetor_ranking_ordenado",vetor_ranking_ordenado,envir = .GlobalEnv)

    cat("---------O nome do seu projeto ?:",projeto,"---------","\n")
    cat("-------- Aternativas do seu projeto:",alternativas,"---------","\n")
    cat("---------Crit?rios do seu projeto:",criterios,"---------","\n")
    cat("---------Os pesos do m?todo sapevo-m s?o:",vetor_peso,"---------","\n\n")
    cat("---------A matriz avalia??o do seu projeto ?:","---------","\n")
    print(matriz_avaliacao)
    cat("\n")
    cat("---------A pontua??o dos crit?rios foi:","---------","\n")
    print(criterios_pontuacao)
    cat("\n")
    cat("---------O ranking do seu projeto ?:","---------","\n")
    print(vetor_ranking_ordenado)

  }
}
