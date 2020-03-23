# Script using Selenium to retrieve 7 Wonders' match stats from top ranked BGA players

from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC 
import time

# Premium account
USERNAME = 'Mineradores'
PASSWORD = ''

# player rank (top 10)
RANK = 1
# player id (keep 0 to use player rank)
USER_ID = 0
# minimum players average level accept
PLAYERS_AVERAGE_LEVEL_MIN = 350
# amount of matches to be taken according to number of players
PLAYERS = {
    3 : 1000,
    4 : 1000,
    5 : 1000,
    6 : 1000,
    7 : 1000
}
# download: https://sites.google.com/a/chromium.org/chromedriver/downloads
WEBDRIVER_PATH = 'C:\\Users\\rvales\\Downloads\\chromedriver_win32\\chromedriver.exe'

def main():
    browser = webdriver.Chrome(WEBDRIVER_PATH)
    wait = WebDriverWait(browser, 10)

    # Log in
    LOGIN_URL = 'https://boardgamearena.com/account'
    browser.get(LOGIN_URL)
    wait.until(EC.presence_of_element_located((By.ID, 'username_input'))).send_keys(USERNAME)
    wait.until(EC.presence_of_element_located((By.ID, 'password_input'))).send_keys(PASSWORD)
    wait.until(EC.element_to_be_clickable((By.ID, 'login_button'))).click()

    # Go to the rank page and search for the player id that will be caught in the matches.
    RANKING_URL = 'https://boardgamearena.com/gamepanel?game=sevenwonders&section=rankings'
    browser.get(RANKING_URL)
    browser.get(RANKING_URL)
    browser.refresh()

    if USER_ID == 0:
        top_players = wait.until(EC.presence_of_element_located((By.XPATH, '//*[@class="gameranking"]'))).find_elements_by_class_name('flag')
        player_id = top_players[RANK-1].get_attribute('id')[5:]
    else:
        player_id = str(USER_ID)

    # Go to player's 7 Wonders matches
    RESULTS_URL = 'https://boardgamearena.com/gamestats?player=' + player_id + '&opponent_id=0&game_id=1131&finished=1'
    browser.get(RESULTS_URL)

    input('\nClick on \'See more\' and load as many matches to save, then press Enter to continue.\n')

    # Get matches ids and store to list games_id
    print('Saving match IDs...')
    games = wait.until(EC.presence_of_element_located((By.XPATH, '//*[@class="statstable"]'))).find_elements_by_class_name('smalltext')
    games_id = []
    for g in games:
        if g.text.startswith('#'):
            games_id.append(g.text[1:])
    total = len(games_id)

    # Stores a list of matches already taken in other runs from matches_saved.txt so as not to repeat them
    open('matches_saved.txt', 'a').close() # create the file if it does not exist yet
    f = open('matches_saved.txt', 'r')
    matches_already_taken = []
    aux = f.readlines()
    for a in aux:
        matches_already_taken.append(a[0:-1])
    f.close()

    # Open file to write the matches that are being picked up in this run
    f = open('matches_saved.txt', 'a')

    # For each match, go to page and...
    print('Acessing ' + str(total) + ' matches:')

    for i in range(total):
        browser.get('https://boardgamearena.com/table?table=' + games_id[i])

        # Check if you need this number of players
        wait.until(EC.presence_of_element_located((By.XPATH, '//*[@class="rank"]')))
        number_players = len(browser.find_elements_by_class_name('rank'))
        if PLAYERS[number_players] <= 0:
            print('[' + str(i+1) + '/' + str(total) + '] Match #' + games_id[i] + ' ignored. ' + str(number_players) + ' player matches are not being sought.')
            continue

        # Checks if the players' level is above the minimum
        #game_stats = wait.until(EC.presence_of_element_located((By.ID, 'table_stats')))
        average_level = int(wait.until(EC.presence_of_element_located((By.XPATH, '//*[@id="table_stats"]/*/*/*/*[@class="gamerank_value"]'))).text)
        if average_level < PLAYERS_AVERAGE_LEVEL_MIN:
            print('[' + str(i+1) + '/' + str(total) + '] Match #' + games_id[i] + ' ignored. Average player level: ' + str(average_level) + ' (min ' + str(PLAYERS_AVERAGE_LEVEL_MIN) + ')')
            continue

        # Checks if the match has already been saved in a previous run
        if games_id[i] in matches_already_taken:
            print('[' + str(i+1) + '/' + str(total) + '] Match #' + games_id[i] + ' has already been saved.')
            continue


        # Get the match stats table
        data = [[], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], [], []]
        table = wait.until(EC.presence_of_element_located((By.ID, 'player_stats_table')))
        table_rows = table.find_elements_by_xpath('//tr')
        for j in range(1, 26):
            v = table_rows[j].find_elements_by_xpath('td')
            # Add the data of each player in the list
            data[j-1].append('')
            for k in range(number_players):
                data[j-1].append(v[k].text)

        # Write to file...
        FILE_NAME = '7wonders_' + str(number_players) + '.csv'

        ff = open(FILE_NAME, 'a')
        # y = each player of each match
        for y in range(len(data[0])):
            # x = each stat (game result, thinking time, vp from.., ...)
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
    
