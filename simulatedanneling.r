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

# função para gerir vizinhos novos formatos da matriz de alocação a fim de encontrar novos vizinhos possíveis
estrutura_vizinhanca <- function(solucao, nhorarios, salas, turmas){
    # guarda a matriz solução
    matriz_vizinhanca <- solucao
    # primeiramente verifica se trocará linha ou coluna
    verifica_troca <- sample(0:1,1)
    # variaives para verificar a quantidade de linhas e colunas
    linhas <- nhorarios
    colunas <- length(salas)
    # variaveis para verificar qual das duas serão trocadas
    linha <- -1
    coluna <- -1
    # instancia o vetor de trocas
    vetor_troca <- vector()
    # caso o número aleatório gerado for 0, troca linha, caso for 1, troca coluna
    if(verifica_troca == 0){
        # agora busca uma linha aleatória entre as linhas possíveis
        linha <- sample(1 : linhas, 1)
        # gera um vetor de valores aleatorios dentro so possíveis pela quantidade de linhas e sem se repetir
        vetor_troca <- sample(1 : linhas, linhas, replace = FALSE)
    }
    else if(verifica_troca == 1){
        # agora busca uma coluna aleátorio dentre as colunas possíveis
        coluna <- sample(1 : colunas, 1)
        # preenche o vetor de valores aleatórios dentro dos possíveis na quantidade de colunas
        vetor_troca <- sample(1 : colunas, colunas, replace = FALSE)
    }
    linha_coluna_solucao <- vector()
    # verifica se linha ou coluna foi selecionada para troca
    if(linha != -1){
        # caso a linha tenha sido preenchdida, pega a linha especifica da matriz
        linha_coluna_solucao <- solucao[linha,]
        # agora realiza a troca
        linha_coluna_solucao <- linha_coluna_solucao[vetor_troca]
        # salva na matriz solução
        matriz_vizinhanca[linha,] <- linha_coluna_solucao
    }
    else if(coluna != -1){
        # caso a coluna tenha sido preenchdida, pega a coluna especifica da matriz
        linha_coluna_solucao <- solucao[,coluna]
        # agora realiza a troca
        linha_coluna_solucao <- linha_coluna_solucao[vetor_troca]
        # salva na matriz solução
        matriz_vizinhanca[,coluna] <- linha_coluna_solucao
    }
    # retorna a matriz modificada
    return (matriz_vizinhanca)
}

# método para executar a função objetiva, que avalia a penalidade e a retorna para ser refinada pelo SA
funcao_objetiva <- function(solucao,nhorarios, salas, turmas){
    penalidade <- 0
    #3 for's para varrer os horarios, salas e turmas
    for(i in 1 : nhorarios){
        for(j in 1 : length(salas)){
            for(k in 1 : length(turmas)){
                # verifica as penalidades da alocação presente da turma
                if(salas[j] > turmas[[k]][3]){
                    #calcula a penalidade 
                    penalidade <- penalidade + (salas[j] - turmas[[k]][3]) * 0.02
                }
                # caso a capacidade da sala for menor que a quantidade de alunos
                else{
                    # penaliza severamente a alocação
                    penalidade <- penalidade + (turmas[[k]][3] - salas[j]) * 300
                }
            }
        }
    }
    # no final retorna a penalidade da alocação
    return (penalidade)
}
#funcao para realizar o simulated anneling de fato, recebe por parametro a temperatura de inicio, a temperatura de parada
# a quantidade de vizinhos máximos a gerar, e as variaveis de tamanho da matriz
simulated_anneling <- function(tinicial, tfinal, alpha, samax, nhorarios, nsalas, nturmas){
    # instancia a matriz da solução inicial
    solucao_atual <- matrix(NA, nhorarios, nsalas)
    # aloca valores aleatórios de para a solução atual, e a guarda também como a melhor até o momento
    solucao_atual <- alocacao_inicial(solucao_atual, nhorarios, salas, turmas)[[2]]
    # guarda sua penalidade como a atual e também a melhor
    penalidade_atual <- funcao_objetiva(solucao_atual, nhorarios, salas, turmas)
    #guarda então os resultados da primeira alocação como a melhor solução até o momento
    solucao_melhor <- solucao_atual
    penalidade_menor <- penalidade_atual
    # inicia a temperatura com o valor da temperatura inicial
    temperatura <- tinicial
    # executa um while até a temperatura ser menor que a temperatura de parada
    while(temperatura > tfinal){
        # agora um laço for para calcular a quantidade de vizinhos de acordo com o SAMAX
        for(i in 1 : samax){
            # usa a função de estrutura de vizinhança para gerar novas possibilidades de vizinhos
            vizinho <- estrutura_vizinhanca(solucao_atual, nhorarios, salas, turmas)
            # pega o valor da função objetiva do vizinho gerado
            penalidade_vizinho <- funcao_objetiva(vizinho, nhorarios, salas, turmas)
            # calcula o delta entre o vizinho e a solução anterior
            delta <- penalidade_vizinho - penalidade_atual
            # verifica se o delta for menor que zera, quer dizr que o vizinho tem uma penalidade melhor que a atual
            if(delta < 0){
                # assim atualiza a solução atual e a penalidade atual
                solucao_atual <- vizinho
                penalidade_atual <- penalidade_vizinho
                # verifica também se o vizinho encontrado, é o melhor de todos
                if(vizinho < penalidade_menor){
                    # atualiza também como melhor de todos, solução e a penalidade
                    solucao_melhor <- vizinho
                    penalidade_menor <- penalidade_vizinho
                }
            }
            # senão for negativo o delta, verifica o criterio de Boltzman para validar se pode aceita-la como uma possível solução ainda
            else{
                # pega a probabilidade a partir da forma (euler^(-delta/temperatura))
				probabilidade <- exp(-delta/temperatura)
                # pega um valor aleatorio para verificar a probabilidade gerada anteriormente
				valor_aleatorio <- runif(1,0,1)
                # se o valor gerado for menor ou igual a probabilidade, quer dizer que a probabilidade foi aceita 
				if(valor_aleatorio <= probabilidade){
					# então como a probabilidade de ser aceita uma solução pior foi aceita, atualiza os valores
                    solucao_atual <- vizinho
                    penalidade_atual <- penalidade_vizinho
				}
			}
        }
        # atualiza a temperatura
        temperatura <- alpha * temperatura
    }
    # retorna no final a melhor solução e a penalidade menor
    r <- list(penalidade_menor, solucao_melhor)
    return (r)
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

nhorarios <- 8
nsalas <- 6
nturmas <- 25
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
print(simulated_anneling(100, 10, 0.99, 5, nhorarios, nsalas, nturmas))
