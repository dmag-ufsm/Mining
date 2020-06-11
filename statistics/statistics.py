import statistics as s

# columns
Place = 0
VP_total = 1
Thinking_time = 2
VP_from_Military_Conflicts_Victory = 3
VP_from_Military_Conflicts_Defeat = 4
VP_from_Treasury_Contents = 5
VP_from_Wonder = 6
VP_from_Civilian_Structures = 7
VP_from_Scientific_Structures = 8
VP_from_Commercial_Structures = 9
VP_from_Guilds = 10
Constructed_stages_of_the_Wonder = 11
Cards_discarded = 12
Chained_constructions = 13
Coins_spent_on_commerce = 14
Coins_gained_through_commerce = 15
Shields = 16
Wonder_side_A = 17
Wonder_ID = 18
Civilian_Structures = 19
Scientific_Structures = 20
Guilds = 21
Military_Structures = 22
Commercial_Structures = 23
Raw_Materials = 24
Manufactured_Goods = 25
Match_ID_Avg_lvl = 26
###

f = open('7wonders_3.csv', 'r')
lines = f.readlines()

n_col = 27
dados = []
for line in lines:
    line = line.strip().split(',')
    if line[0] == '1' or line[0] == '2' or line[0] == '3':
        dados.append(line)


# Estatisticas como min, max, desvio padrao, ...
def common_statistics():
    float_prec = 4

    print('coluna,minimo,maximo,media,desvio padrao,varianca,1 quartil,mediana,3 quartil')
    for i in range(26):
        valores = []
        
        for j in range(len(dados)):
            
            if i == Thinking_time:
                t = dados[j][i].split(':')
                v = int(t[0]) * 60 + int(t[1])

            elif i == Wonder_side_A:
                v = 1 if dados[j][i] == 'yes' else 0

            elif i == Match_ID_Avg_lvl:
                v = 0

            else:
                v = int(dados[j][i])

            valores.append(v)

        minimo = str(min(valores))
        maximo = str(max(valores))
        media = str(s.mean(valores))
        desvio = str(s.pstdev(valores))
        varianca = str(s.pvariance(valores))
        valores.sort()
        quartil1 = str(valores[round(len(valores)*0.25)])
        mediana = str(valores[round(len(valores)*0.5)])
        quartil3 = str(valores[round(len(valores)*0.75)])

        print(str(i) + ',' +
              minimo + ',' +
              maximo + ',' +
              media[:media.find('.')+float_prec+1] + ',' +
              desvio[:desvio.find('.')+float_prec+1] + ',' +
              varianca[:varianca.find('.')+float_prec+1] + ',' +
              quartil1 + ',' +
              mediana + ',' +
              quartil3)


def wonder_statistics():
    float_prec = 2

    print('Wonder ID,Uses,Wins,Win rate,Place,VP total,Thinking time,VP from Military Conflicts (Victory),VP from Military Conflicts (Defeat),VP from Treasury Contents,VP from Wonder, VP from Civilian Structures,VP from Scientific Structures,VP from Commercial Structures,VP from Guilds,Constructed stages of the Wonder,Cards discarded,Chained constructions,Coins spent on commerce,Coins gained through commerce,Shields,Civilian Structures,Scientific Structures,Guilds,Military Structures,Commercial Structures,Raw Materials,Manufactured Goods')

    for w_id in range(1, 15):
        contUsos = 0
        contVitorias = 0
        for i in range(len(dados)):
            if int(dados[i][Wonder_ID]) == w_id:
                contUsos = contUsos + 1
                if dados[i][Place] == '1':
                    contVitorias = contVitorias + 1
        print(w_id, end=',')
        print(contUsos, end=',')
        print(contVitorias, end=',')
        print(round(100*contVitorias / contUsos, 2), end=',')

        for i in range(26):
            valores = []
            
            for j in range(len(dados)):
                
                if int(dados[j][Wonder_ID]) != w_id:
                    continue

                if i == Thinking_time:
                    t = dados[j][i].split(':')
                    v = int(t[0]) * 60 + int(t[1])

                elif i == Wonder_side_A or i == Wonder_ID or i == Match_ID_Avg_lvl:
                    continue

                else:
                    v = int(dados[j][i])

                valores.append(v)

            if len(valores) > 0:
                media = str(s.mean(valores))
                print(media[:media.find('.')+float_prec+1], end='')

                if i < 25:
                    print(',', end='')
                else:
                    print()


# estrategias

def wonder_vp_sci():
    num_max = 9

    tot = num_max*[0]
    tudo = 0

    print('Wonder ID,', end='')
    for i in range(num_max):
        print(i, end='')
        if i < num_max - 1:
            print(',', end='')
        else:
            print(',total')

    for w_id in range(1, 15):
        total_wonder = 0
        cont = num_max * [0]

        for d in dados:

            if int(d[Wonder_ID]) != w_id or d[Place] != '1':
                continue

            pontos_ciencia = int(int(d[VP_from_Scientific_Structures]) / 10)

            if int(d[VP_from_Scientific_Structures]) < int(d[VP_total])*0.35:
                continue

            cont[pontos_ciencia] = cont[pontos_ciencia] + 1
            tot[pontos_ciencia] = tot[pontos_ciencia] + 1
            tudo = tudo + 1
            total_wonder = total_wonder + 1

        print(w_id, end='')
        print(',', end='')

        for i in range(len(cont)):
            print(cont[i], end='')

            if i < len(cont) - 1:
                print(',', end='')
            else:
                print(','+str(total_wonder))

    print('total,', end='')
    for i in range(len(tot)):
            print(tot[i], end='')

            if i < len(tot) - 1:
                print(',', end='')
            else:
                print(','+str(tudo))

    print('rate,', end='')
    for i in range(len(tot)):
            print(round(100*tot[i]/tudo, 2), end='')

            if i < len(tot) - 1:
                print(',', end='')
            else:
                print(',-')


def wonder_vp_military():
    num_max = 19

    min_pontos = 10

    tot = num_max*[0]
    tudo = 0

    print('Wonder ID,', end='')
    for i in range(min_pontos, num_max):
        print(i, end='')
        if i < num_max - 1:
            print(',', end='')
        else:
            print(',total')

    for w_id in range(1, 15):
        total_wonder = 0
        cont = num_max * [0]

        for d in dados:

            if int(d[Wonder_ID]) != w_id or d[Place] != '1':
                continue

            pontos_militar = int(d[VP_from_Military_Conflicts_Victory]) + int(d[VP_from_Military_Conflicts_Defeat])

            if pontos_militar < 10:
                continue

            cont[pontos_militar] = cont[pontos_militar] + 1
            tot[pontos_militar] = tot[pontos_militar] + 1
            tudo = tudo + 1
            total_wonder = total_wonder + 1

        print(w_id, end='')
        print(',', end='')

        for i in range(min_pontos, len(cont)):
            print(cont[i], end='')

            if i < len(cont) - 1:
                print(',', end='')
            else:
                print(','+str(total_wonder))

    print('total,', end='')
    for i in range(min_pontos, len(tot)):
            print(tot[i], end='')

            if i < len(tot) - 1:
                print(',', end='')
            else:
                print(','+str(tudo))

    print('rate,', end='')
    rate_total = 0.0
    for i in range(min_pontos, len(tot)):
            rate = round(100*tot[i]/tudo, 2)
            rate_total = rate_total + rate

            print(rate, end='')

            if i < len(tot) - 1:
                print(',', end='')
            else:
                print(','+str(round(rate_total, 2)))


def shields_statistics():
    num_max = 13

    print('Shields,VP from Conflicts Victory,VP from Conflicts Defeat,Balance,VPs/Shields')

    cont = num_max * [0]

    for i in range(num_max):
        
        valor_vic = []
        valor_def = []

        for d in dados:

            if int(d[Shields]) != i:
                continue

            valor_vic.append(int(d[VP_from_Military_Conflicts_Victory]))
            valor_def.append(int(d[VP_from_Military_Conflicts_Defeat]))

        media_vic = 0 if len(valor_vic) == 0 else round(s.mean(valor_vic), 2)
        media_def = 0 if len(valor_def) == 0 else round(s.mean(valor_def), 2)

        print(i, end=',')
        print(media_vic, end=',')
        print(media_def, end=',')
        print(round(media_vic + media_def, 2), end=',')
        print(round(round(media_vic + media_def, 2) / (1 if i == 0 else i)), end='\n')

def main():
    wonder_statistics()

main()