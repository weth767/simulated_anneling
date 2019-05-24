#Funcao para fazer a primeira alocacao de turmas em suas salas
alocacao_inicial <- function(solucao, nhorarios, salas, turmas){
    #variavel para guardar a penalidade
    penalidade <- 0
    #varre a quantidade de horarios disponivel
    for(i in 1 : nhorarios){
        #varre a quantidade de salas disponiveis
        for(j in 1 : length(salas)){
            #varre a quantidade de turmas
            for(k in 1 : length(turmas)){
                #se nao foi alocado turma para a sala num determinado horario
                if(is.na(solucao[i,j])){
                    #caso a turma nao tenha sido alocada (POSICAO 4 indica que ainda nao teve sala alocada para a turma)
                    if(turmas[[k]][4] == 0){
                        #verifica se a sala tem capacidade para a turma
                        if(salas[j] > turmas[[k]][3]){
                            #calcula a penalidade 
                            penalidade <- penalidade + (salas[j] - turmas[[k]][3]) * 0.02
                        }
                        #caso nao cabe a quantidade de alunos na sala
                        else{
                            #calcula uma penalidade maior
                            penalidade <- penalidade + (turmas[[k]][3] - salas[j]) * 300
                        }
                        #caso so precise alocar um horario
                        if(turmas[[k]][2] == 1){
                            #aloca 1 horario para a turma
                            solucao[i,j] <- turmas[[k]][1]
                        }
                        #caso precise alocar dois horario
                        else if(turmas[[k]][2] == 2){
                            #aloca o primeiro horario
                            solucao[i,j] <- turmas[[k]][1]
                            # verifica se cabe a aula
                            if(i + 1 <= nhorarios){
                                 #aloca o segundo horario
                                solucao[i + 1,j] <- turmas[[k]][1]
                            }
                            # senão cabe, não insere a turma, entretanto aplica penalidade pelo número de aulas que não 
                            # puderam ser inseridas
                            else{
                                penalidade <- penalidade + 300
                            } 
                        }
                        #caso precise alocar tres horario
                        else if(turmas[[k]][2] == 3){
                            #aloca o primeiro
                            solucao[i,j] <- turmas[[k]][1]
                            # verifica se cabe a aula
                            if(i + 1 <= nhorarios){
                                 #aloca o segundo horario
                                solucao[i + 1,j] <- turmas[[k]][1]
                            }
                            # senão cabe, aumenta a penalidade
                            else{
                                penalidade <- penalidade + 300
                            }
                            # verifica da mesma forma, se cabe a aula naquele horário
                            if(i + 2 <= nhorarios){
                                #e o terceiro horario
                                solucao[i + 2, j] <- turmas[[k]][1]
                            }
                            # da mesma forma, se não couber, aumenta a penalidade
                            else{
                                penalidade <- penalidade + 300
                            } 
                        }
                        #coloca que a turma ja foi alocada com seus horarios
                        turmas[[k]][4] <- TRUE
                    }
                }
            }
        }
    }
    #retorna uma lista contendo a solucao inicial e a sua penalidade
    r <- list(penalidade, solucao)
    return (r)
}

funcao_objetiva <- function(solucao,nhorarios,turmas, salas){
    
}
#funcao para realizar o simulated anneling
simulated_anneling <- function(tinicial, tfinal, alpha, samax, nhorarios, nsalas, nturmas){
    solucao_atual <- matrix(NA, nhorarios, nsalas)
    solucao_atual <- alocacao_inicial(solucao_atual, nhorarios, salas, turmas)[[2]]
    print(solucao_atual)
    while(FALSE){
        
    }
}
#funcao para inserir as informacoes da turma em uma lista
preenche_list_turmas <- function(vetor_aulas, vetor_alunos, turmas){
    #declara a lista de turma
    list_turmas <- list()
    #for para varrer a quantidade de turmas
    for(i in 1 : turmas){
        #insere um vetor de 4 posicoes com NA
        vetor_turma <- c(NA, 4)
        #nome da turma  (ID da turma)
        vetor_turma[1] <- i
        #Quantidade de aulas da turma
        vetor_turma[2] <- vetor_aulas[i]
        #quantidade de alunos da turma
        vetor_turma[3] <- vetor_alunos[i]
        #identificador para saber se a turma foi alocada ou nao
        vetor_turma[4] <- FALSE
        #insere a turma na lista
        list_turmas[[i]] <- vetor_turma
    }
    #retorna a lista de turmas
    return(list_turmas)
}

nhorarios <- 5
nsalas <- 5
nturmas <- 10
solucao_melhor <- NA
penalidade_menor <- NA

# instancia o vetor de salas com suas capacidades
salas <- sample(25:60, nsalas)
# instancia o vetor da quantidade de aulas de cada turma
aulas <- sample(x = 1:3, size = nturmas, replace = TRUE)
# instancia o vetor com a quantidade de alunos da turma
alunos <- sample(x =  10:60, size = nturmas, replace = TRUE)
# recebe o vetor final de turmas com as informações da turma, nome, quantidade de aulas e de alunos
turmas <- preenche_list_turmas(aulas, alunos, nturmas)
simulated_anneling(100, 10, 0.99, 5, nhorarios, nsalas, nturmas)
