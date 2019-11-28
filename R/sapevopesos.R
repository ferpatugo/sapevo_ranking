#' @title metodo multicriterio
#'
#' @description apoio a tomada de decisao
#'
#' @param symbol
#'
#' @return NULL
#'
#' @examples teste
#'
#' @export sapevo_pesos

sapevo_pesos=function(projeto,decisores,alternativas,criterios,
                      #--entrada nota decisores para o peso
                      vetor_notas_decisor1=NULL,vetor_notas_decisor2=NULL,
                      vetor_notas_decisor3=NULL,vetor_notas_decisor4=NULL,
                      vetor_notas_decisor5=NULL,vetor_notas_decisor6=NULL,
                      vetor_notas_decisor7=NULL,vetor_notas_decisor8=NULL,
                      vetor_notas_decisor9=NULL,vetor_notas_decisor10=NULL

) {

  listas_notas_decisores=list(vetor_notas_decisor1,
                              vetor_notas_decisor2,
                              vetor_notas_decisor3,
                              vetor_notas_decisor4,
                              vetor_notas_decisor5,
                              vetor_notas_decisor6,
                              vetor_notas_decisor7,
                              vetor_notas_decisor8,
                              vetor_notas_decisor9,
                              vetor_notas_decisor10
  )

  numero_criterios=length(criterios)
  numero_alternativas=length(alternativas)

  vetor_peso=data.frame()

  for (i in 1:length(listas_notas_decisores)) {

    if (is.null(listas_notas_decisores[[i]])==F) {

      decisorpeso = matrix(listas_notas_decisores[[i]],
                           nrow=numero_criterios,
                           ncol=numero_criterios,
                           byrow = TRUE)

      vetorpesonorm=rowSums(data.frame(decisorpeso))
      vetorpesonorm=(vetorpesonorm-min(vetorpesonorm))/(max(vetorpesonorm)-min(vetorpesonorm))
      vetorpesonorm=ifelse(vetorpesonorm==0,0.01*tail(sort(vetorpesonorm),2)[1],vetorpesonorm)
      vetor_peso=colSums(rbind(vetor_peso,vetorpesonorm))
      names(vetor_peso)=criterios
      assign("vetor_peso",vetor_peso,envir = .GlobalEnv)



    } else {

      cat("")

    }
  }
  cat("O nome do seu projeto ?:",projeto,"\n")
  cat("Aternativas do seu projeto:",alternativas,"\n")
  cat("Criterios do seu projeto:",criterios,"\n")
  cat("Os pesos do metodo sapevo-m sãoo:",vetor_peso)
}

