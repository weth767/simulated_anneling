#Funcao para fazer a primeira alocacao de turmas em suas salas
alocacao_inicial <- function(solucao, nhorarios, salas, turmas){
    #variavel para guardar a penalidade
    penalidade <- 0
    #varre a quantidade de horarios disponivel
    for(k in 1 : length(turmas[[1]])){
        #varre a quantidade de salas disponiveis
        for(i in 1 : nhorarios){
            #varre a quantidade de turmas
            for(j in 1 : length(salas[[1]])){
                #se nao foi alocado turma para a sala num determinado horario
                if(is.na(solucao[i,j])){
                    #caso a turma nao tenha sido alocada (POSICAO 4 indica que ainda nao teve sala alocada para a turma)
                    if(identical(turmas[[4]][k],FALSE)){
                        #verifica se a sala tem capacidade para a turma
                        if(salas[[2]][j] > turmas[[3]][k]){
                            #calcula a penalidade 
                            penalidade <- penalidade + (salas[[2]][j] - turmas[[3]][k]) * 0.02
                        }
                        #caso nao cabe a quantidade de alunos na sala
                        else{
                            #calcula uma penalidade maior
                            penalidade <- penalidade + (turmas[[3]][k] - salas[[2]][j]) * 100
                        }
                        #caso so precise alocar um horario
                        if(turmas[[2]][k] == 2){
                           #aloca o primeiro horario
                            solucao[i,j] <- turmas[[1]][k]
                            # verifica se cabe a aula
                            if(i + 1 <= nhorarios){
                                 #aloca o segundo horario
                                solucao[i + 1, j] <- turmas[[1]][k]
                            }
                            # senão cabe, não insere a turma, entretanto aplica penalidade pelo número de aulas que não 
                            # puderam ser inseridas
                            else{
                                penalidade <- penalidade + 100
                            }
                        }
                        #caso precise alocar quatros horarios de aula
                        else if(turmas[[2]][k] == 4){
                            #aloca o primeiro horario
                            solucao[i,j] <- turmas[[1]][k]
                            # um for agora para alocar as outras aulas
                            for(z in (i + 1) : (i + 3)){
                                 # verifica se cabe a aula
                                if(z <= nhorarios){
                                    solucao[z,j] <- turmas[[1]][k]
                                }
                                # senão cabe, não insere a turma, entretanto aplica penalidade pelo número de aulas que não puderam ser inseridas
                                else{
                                    penalidade <- penalidade + 100
                                }
                            }
                        }
                        #caso precise alocar seis horarios de aula
                        else if(turmas[[2]][k] == 6){ 
                            #aloca o primeiro
                            solucao[i,j] <- turmas[[1]][k]
                           # um for agora para alocar as outras aulas
                            for(z in (i + 1) : (i + 5)){
                                 # verifica se cabe a aula
                                if(z <= nhorarios){
                                    solucao[z,j] <- turmas[[1]][k]
                                }
                                # senão cabe, não insere a turma, entretanto aplica penalidade pelo número de aulas que não puderam ser inseridas
                                else{
                                    penalidade <- penalidade + 100
                                }
                            }
                        }
                        else if(turmas[[2]][k] == 8){
                            #aloca o primeiro
                            solucao[i,j] <- turmas[[1]][k]
                            # um for agora para alocar as outras aulas
                            for(z in (i + 1) : (i + 7)){
                                 # verifica se cabe a aula
                                if(z <= nhorarios){
                                    solucao[z,j] <- turmas[[1]][k]
                                }
                                # senão cabe, não insere a turma, entretanto aplica penalidade pelo número de aulas que não puderam ser inseridas
                                else{
                                    penalidade <- penalidade + 300
                                }
                            }
                        }
                        #coloca que a turma ja foi alocada com seus horarios
                        turmas[[4]][k] <- TRUE
                    }
                }
            }
        }
    }
    # retorna a solucao inicial
    return (solucao)
}

# função para gerir vizinhos novos formatos da matriz de alocação a fim de encontrar novos vizinhos possíveis
estrutura_vizinhanca_linha <- function(solucao, nhorarios, salas, turmas){
    # guarda a matriz solução
    matriz_vizinhanca <- solucao
    # variaives para verificar a quantidade de linhas e colunas
    linhas <- nhorarios
    colunas <- length(salas)
    # agora busca uma linha aleatória entre as linhas possíveis
    linha <- sample(1 : linhas, 1)
    # verifica se linha ou coluna foi selecionada para troca
    # caso a linha tenha sido preenchdida, pega a linha especifica da matriz
    linha_solucao <- matriz_vizinhanca[linha,]
    troca <- sample(1 : colunas, colunas, replace = FALSE)
    linha_solucao <- linha_solucao[troca]
    matriz_vizinhanca[linha,] <- linha_solucao
    # retorna a matriz modificada
    return (matriz_vizinhanca)
}

estrutura_vizinhanca_coluna <- function(solucao, nhorarios, salas, turmas){
    # guarda a matriz solução
    matriz_vizinhanca <- solucao
    # variaives para verificar a quantidade de linhas e colunas
    linhas <- nhorarios
    colunas <- length(salas[1])
    # agora busca uma coluna aleatória entre as colunas possíveis
    coluna <- sample(1 : colunas, 1)
    # verifica se coluna ou coluna foi selecionada para troca
    # caso a coluna tenha sido preenchdida, pega a coluna especifica da matriz
    coluna_solucao <- matriz_vizinhanca[,coluna]
    troca <- sample(1 : linhas, linhas, replace = FALSE)
    coluna_solucao <- coluna_solucao[troca]
    matriz_vizinhanca[,coluna] <- coluna_solucao
    # retorna a matriz modificada
    return (matriz_vizinhanca)
}

# método para executar a função objetiva, que avalia a penalidade e a retorna para ser refinada pelo SA
funcao_objetiva <- function(solucao,nhorarios, salas, turmas){
    penalidade <- 0
    #3 for's para varrer os horarios, salas e turmas
    for(i in 1 : nhorarios){
        for(j in 1 : length(salas[[1]])){
            for(k in 1 : length(turmas[[1]])){
                # verifica as penalidades da alocação presente da turma
                if(!(is.na(solucao[i,j]))){
                    if(solucao[i,j] == turmas[[1]][k]){
                        if(salas[[2]][j] > turmas[[3]][k]){
                            #calcula a penalidade 
                            penalidade <- penalidade + (salas[[2]][j] - turmas[[3]][k]) * 0.02
                        }
                        # caso a capacidade da sala for menor que a quantidade de alunos
                        else{
                            # penaliza severamente a alocação
                            penalidade <- penalidade + (turmas[[3]][k] - salas[[2]][j]) * 300
                        }
                    }
                }
            }
        }
    }
    # no final retorna a penalidade da alocação
    return (penalidade)
}
#funcao para realizar o simulated anneling de fato, recebe por parametro a temperatura de inicio, a temperatura de parada
# a quantidade de vizinhos máximos a gerar, e as variaveis de tamanho da matriz
simulated_anneling <- function(tinicial, tfinal, alpha, samax, nhorarios, salas, turmas){
    # pega o tamanho das listas
    nsalas <- length(salas[[1]])
    nturmas <- length(turmas[[1]])
    # instancia a matriz da solução inicial
    solucao_atual <- matrix(NA, nhorarios, nsalas)
    # aloca valores aleatórios de para a solução atual, e a guarda também como a melhor até o momento
    solucao_atual <- alocacao_inicial(solucao_atual, nhorarios, salas, turmas)
    print(solucao_atual)
    # guarda sua penalidade como a atual e também a melhor
    penalidade_atual <- funcao_objetiva(solucao_atual, nhorarios, salas, turmas)
    print(penalidade_atual)
    #guarda então os resultados da primeira alocação como a melhor solução até o momento
    solucao_melhor <- solucao_atual
    penalidade_menor <- penalidade_atual
    # guarda as soluções
    Y <- c(penalidade_atual)
    # inicia a temperatura com o valor da temperatura inicial
    temperatura <- tinicial
    # executa um while até a temperatura ser menor que a temperatura de parada
    while(temperatura > tfinal){
        # agora um laço for para calcular a quantidade de vizinhos de acordo com o SAMAX
        for(i in 1 : samax){
            verifica_troca <- sample(0:1,1)
            if(verifica_troca == 0){
                # usa a função de estrutura de vizinhança para gerar novas possibilidades de vizinhos
                vizinho <- estrutura_vizinhanca_linha(solucao_atual, nhorarios, salas, turmas)
            }
            else if(verifica_troca == 1){
                # usa a função de estrutura de vizinhança para gerar novas possibilidades de vizinhos
                vizinho <- estrutura_vizinhanca_coluna(solucao_atual, nhorarios, salas, turmas)
            }
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
                if(penalidade_vizinho < penalidade_menor){
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
            # Atualiza os vetores para plot do grafico
			Y <- c(Y, penalidade_atual)
        }
        # atualiza a temperatura
        temperatura <- alpha * temperatura
    }
    # plota a variação de resultados 
    png('evolucao-sa.png', width=1600)
	plot(1:length(Y), Y, type='l', col='red', main='Evolucao do SA no tempo', xlab='Iteracao', ylab='Custo')
	dev.off()
    # retorna no final a melhor solução e a penalidade menor
    r <- list(solucao_melhor,penalidade_menor)
    return (r)
}

# instancia os vetores de id das salas e suas capacidades
id_salas <- c(15,16,21,22,23,24,25,26,27,31,32,33,34,35,36,37,38,39)
capacidades_salas <- c(44,81,57,55,33,34,41,82,44,40,35,33,23,31,36,41,64,40)
# depois joga em uma lista para ser acessada facilmente no método do simulate anneling
salas <- list(id_salas,capacidades_salas)
# faz o mesmo com as turmas(1 a 4 tecnico em computação, 11 a 14 técnico em eletrotecnica, 21 a 24 técnico em 
# administração, 31 a 34 bacharel em administração, 41 a 44, bacharel em engenharia elétrica e 51 a 54, bacharel em computação)
id_turmas <- c(1,2,3,4,11,12,13,14,21,22,23,24,31,32,33,34,41,42,43,44,51,52,53,54)
qnt_aulas <- c(4,4,2,4,2,4,6,6,4,4,2,2,4,6,6,2,4,6,4,4,2,4,6,4)
qnt_alunos <- sample(20:60,length(id_turmas))
estado_alocacao <- c(rep(FALSE,length(id_turmas)))
#aulas_naoalocadas <- c(rep(NA,length(id_turmas)))
# adiciona depois tudo na lista de turmas
turmas <- list(id_turmas, qnt_aulas, qnt_alunos, estado_alocacao)

nhorarios <- 10
print(simulated_anneling(1000, 10, 0.99, 5, nhorarios, salas, turmas))
#solucao <- matrix(NA, nhorarios, length(salas[[1]]))
#print(alocacao_inicial(solucao, nhorarios, salas, turmas))
