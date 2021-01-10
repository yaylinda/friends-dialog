import csv
import json


def write_csv(data):
  """ Writes a CSV file of the ratings data """

  with open('data.csv', 'w') as file:
    writer = csv.DictWriter(file, fieldnames=data[0].keys())
    writer.writeheader()
    for row in data:
      writer.writerow(row)


def parse_json(season, json_file):
  """ Parses a JSON object reprenting one season, creating a list of lines spoken by each character for each episode."""

  season_data = []

  for episode in json_file['episodes']:
    episode_num = int(episode['episode_id'].split('_e')[1])
    for scene in episode['scenes']:
      for utterance in scene['utterances']:
        for speakers in utterance['speakers']:
          transcript = utterance['transcript']
          season_data.append({
            'season': season,
            'episode': episode_num,
            'transcript': transcript,
            'num_words': len(transcript.split(' '))
          })
          
  return season_data


def main():
  num_seasons = 10
  json_filename_format = 'json/friends_season_%s.json'

  data = []
  for i in range(1, num_seasons + 1):
    filename = json_filename_format % str(i)
    print('Opening file: %s' % filename)

    with open(filename) as json_file:
      json_data = json.load(json_file)
      season_data = parse_json(i, json_data)

      data.extend(season_data)

  print('Exporting data to data.csv...')
  write_csv(data)

  print('Done!')
  

if __name__ == '__main__':
  main()