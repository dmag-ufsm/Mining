# Script com Selenium para resgatar estatisticas de partidas de 7 Wonders dos melhores jogadores do BGA

from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC 
import time

# conta premium
USERNAME = 'Mineradores'
PASSWORD = 'dmag1204b'

# rank do jogador a ser buscada as partidas (do top 10)
RANK = 1
# buscar partidas de um jogador especifico (deixar 0 para buscar a partir do rank aqui de cima)
USER_ID = 0
# minimo aceito de media de pontos dos jogadores da partida
PLAYERS_AVERAGE_LEVEL_MIN = 300
# quantia de partidas buscadas por numero de jogadores
PLAYERS = {
    3 : 0,
    4 : 0,
    5 : 0,
    6 : 176,
    7 : 112
}
# Webdriver em https://sites.google.com/a/chromium.org/chromedriver/downloads
WEBDRIVER_PATH = 'C:\\Users\\rvales\\Downloads\\chromedriver_win32\\chromedriver.exe'

def main():
    browser = webdriver.Chrome(WEBDRIVER_PATH)
    wait = WebDriverWait(browser, 10)

    # Acessa e efetua login no site
    LOGIN_URL = 'https://boardgamearena.com/account'
    browser.get(LOGIN_URL)
    wait.until(EC.presence_of_element_located((By.ID, 'username_input'))).send_keys(USERNAME)
    wait.until(EC.presence_of_element_located((By.ID, 'password_input'))).send_keys(PASSWORD)
    wait.until(EC.element_to_be_clickable((By.ID, 'login_button'))).click()

    # Acessa o ranking e busca o id do jogador que sera buscada as partidas
    RANKING_URL = 'https://boardgamearena.com/gamepanel?game=sevenwonders&section=rankings'
    browser.get(RANKING_URL)
    browser.get(RANKING_URL)
    browser.refresh()

    if USER_ID == 0:
        top_players = wait.until(EC.presence_of_element_located((By.XPATH, '//*[@class="gameranking"]'))).find_elements_by_class_name('flag')
        player_id = top_players[RANK-1].get_attribute('id')[5:]
    else:
        player_id = str(USER_ID)

    # Aessa as partidas de 7 Wonders do jogador
    RESULTS_URL = 'https://boardgamearena.com/gamestats?player=' + player_id + '&opponent_id=0&game_id=1131&finished=1'
    browser.get(RESULTS_URL)

    input('\nClick on \'See more\' and load as many matches to save, then press Enter to continue.\n')

    # Resgata o id das partidas e armazena na lista games_id
    print('Saving match IDs...')
    games = wait.until(EC.presence_of_element_located((By.XPATH, '//*[@class="statstable"]'))).find_elements_by_class_name('smalltext')
    games_id = []
    for g in games:
        if g.text.startswith('#'):
            games_id.append(g.text[1:])
    total = len(games_id)

    # Cria uma lista das partidas ja registradas em outras execucoes a partir de matches_saved.txt para nao repeti-las
    open('matches_saved.txt', 'a').close() # cria o arquivo caso nao exista ainda
    f = open('matches_saved.txt', 'r')
    matches_already_taken = []
    aux = f.readlines()
    for a in aux:
        matches_already_taken.append(a[0:-1])
    f.close()

    # Abre arquivo para escrever as partidas que vao sendo pegas nessa execucao
    f = open('matches_saved.txt', 'a')

    # Para cada partida, acessa a pagina dela e...
    print('Acessing ' + str(total) + ' matches:')

    for i in range(0, total):
        browser.get('https://boardgamearena.com/table?table=' + games_id[i])

        # Verifica se esta sendo buscada partidas com essa quantidade de jogadores
        wait.until(EC.presence_of_element_located((By.XPATH, '//*[@class="rank"]')))
        number_players = len(browser.find_elements_by_class_name('rank'))
        if PLAYERS[number_players] <= 0:
            print('[' + str(i+1) + '/' + str(total) + '] Match #' + games_id[i] + ' ignored. ' + str(number_players) + ' player matches are not being sought.')
            continue

        # Verifica se o nivel dos jogadores da partida esta acima do minimo definido
        game_stats = wait.until(EC.presence_of_element_located((By.ID, 'table_stats')))
        average_level = int(wait.until(EC.presence_of_element_located((By.XPATH, '//*[@id="table_stats"]/*/*/*/*[@class="gamerank_value"]'))).text)
        if average_level < PLAYERS_AVERAGE_LEVEL_MIN:
            print('[' + str(i+1) + '/' + str(total) + '] Match #' + games_id[i] + ' ignored. Average player level: ' + str(average_level) + ' (min ' + str(PLAYERS_AVERAGE_LEVEL_MIN) + ')')
            continue

        # Verifica se a partida ja foi salva em uma execucao anterior
        if games_id[i] in matches_already_taken:
            print('[' + str(i+1) + '/' + str(total) + '] Match #' + games_id[i] + ' has already been saved.')
            continue


        # Pega a tabela de estatisticas da partida
        data = [[], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], []]
        table = wait.until(EC.presence_of_element_located((By.ID, 'player_stats_table')))
        table_rows = table.find_elements_by_xpath('//tr')
        for j in range(1, 26):
            v = table_rows[j].find_elements_by_xpath('td')
            # Insere o dado de cada jogador na lista para posteriormente ir ao csv
            data[j-1].append('')
            for k in range(0, number_players):
                data[j-1].append(v[k].text)

        # Escreve no arquivo
        FILE_NAME = '7wonders_' + str(number_players) + '.csv'

        ff = open(FILE_NAME, 'a')
        # y = cada jogador de cada partida
        for y in range(0, len(data[0])):
            # x = cada stat (game result, thinking time, vp from.., etc)
            for x in range(0, 25):
                ff.write(data[x][y])
                if x != 24:
                    ff.write(',')
            ff.write('\n')
        ff.close()

        PLAYERS[number_players] -= 1
        print('[' + str(i+1) + '/' + str(total) + '] Match #' + games_id[i] + ' saved to ' + FILE_NAME + ' (' + str(PLAYERS[number_players]) + ' more matches for ' + str(number_players) + ' players)')
        f.write(games_id[i] + '\n')

    browser.quit()
    f.close()

    print('\nMissing:')
    for n in range(3, 8):
        if PLAYERS[n] > 0:
            print('' + str(PLAYERS[n]) + ' matches for ' + str(n) + ' players') 

if __name__ == '__main__':
    main()
