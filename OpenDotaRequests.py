import requests as R
import json
import numpy as np

KEYS = ['match_id','player_slot','assists','deaths','denies','firstblood_claimed','gold','gold_per_min','gold_spent','hero_damage','hero_healing','hero_id','item_0','item_1','item_2','item_3','item_4','item_5','kills','last_hits','level','obs_placed','roshans_killed','stuns','teamfight_participation','tower_damage','towers_killed','xp_per_min','isRadiant','total_gold','total_xp','kills_per_min','kda','lane_efficiency','lane','lane_role','is_roaming','actions_per_min']
current = 0

def remove_null(target):
	target = {k: v for k, v in target.items() if v is not None}
	for x in target.keys():
		if (type(target[x]) == type({})):
			target[x] = {k: v for k, v in target[x].items() if v is not None}
		if (type(target[x]) == type([])):
			target[x] = list(filter(None, target[x]))
			for t in target[x]:
				if (type(t) == type({})):
					t = {k: v for k, v in t.items() if v is not None}
	return target

def get_player_list(match_data):
	llista = np.zeros(shape=(10,len(KEYS)))
	i = 0
	for p in match_data['players']: 
		ks = [k for k, v in p.items() if k in KEYS]
		print(ks)
		aux = [v for k, v in p.items() if k in KEYS]
		print(aux)
		llista[i] = aux 
		print()
		print(llista[i])
		print(llista[i].shape)
		print()
		i = i + 1
	print(llista.shape)
	return np.array(llista)

def get_info(match_id):
	match = R.get('https://api.opendota.com/api/matches/' + str(match_id))
	match_data = match.json()
	match_data = remove_null(match_data)

	players = get_player_list(match_data)
	victory = np.array(match_data['radiant_win'])
	return players,victory

def get_next_info(size):
	global current
	pro = R.get('https://api.opendota.com/api/proMatches/')
	data = pro.json()

	x = np.zeros(shape=(size,10,len(KEYS)))
	y = np.zeros(shape=(size,1))

	i = 0


def main():
	pro = R.get('https://api.opendota.com/api/proMatches/')
	data = pro.json()
	print(data[98])
	print(data[99])
	#players,victory = get_info(data[0]['match_id'])
	#print(players)
	#print(victory)

main()