setwd("/Users/lindazheng/Developer/friends-dialog")

library(ggplot2)
library(RColorBrewer)
library(ggthemes)
library(treemapify)
library(stringr)

data = read.csv("data.csv")
data$character[data$character == "Rachel Green"] = "Rachel"
data$character[data$character == "Ross Geller"] = "Ross"
data$character[data$character == "Chandler Bing"] = "Chandler"
data$character[data$character == "Joey Tribbiani"] = "Joey"
data$character[data$character == "Monica Geller"] = "Monica"
data$character[data$character == "Phoebe Buffay"] = "Phoebe"
data$character[data$character == "Amy Green"] = "Amy G."

num_words_per_character = aggregate(
  data$num_words,
  by = list(
    character = data$character
  ),
  sum
)

words_per_character_per_episode = aggregate(
  data$num_words,
  by = list(
    character = data$character,
    season = data$season,
    episode = data$episode
  ),
  sum
)

most_words_per_episode = data.frame(c(), c(), c(), c())

for (seasonNum in 1:max(data$season)) {
  for (episodeNum in 1:max(data[which(data$season == seasonNum), ]$episode)) {
    
    season_subset = words_per_character_per_episode[
      which(words_per_character_per_episode$season == seasonNum), 
    ]
    
    season_episode_subset = season_subset[
      which(season_subset$episode == episodeNum), 
    ]
    
    max_words = max(season_episode_subset$x)
    
    character = season_episode_subset[
      which(season_episode_subset$x == max_words), 
    ]$character
    
    most_words_per_episode = rbind(
      most_words_per_episode, 
      c(seasonNum, episodeNum, max_words, character)
    )
  }
}

colnames(most_words_per_episode) = c("season", "episode", "words_spoken", "character")

most_words_per_episode$season = as.numeric(most_words_per_episode$season)
most_words_per_episode$episode = as.numeric(most_words_per_episode$episode)
most_words_per_episode$words_spoken = as.numeric(most_words_per_episode$words_spoken)
most_words_per_episode$character = factor(
  most_words_per_episode$character,
  levels = num_words_per_character[order(-num_words_per_character$x),]$character
)
most_words_per_episode = subset(
  most_words_per_episode, 
  most_words_per_episode$words_spoken > 0
)


cumulative_words_per_character = aggregate(
  data$num_words,
  by = list(
    character = data$character
  ),
  sum
)
cumulative_words_per_character = subset(
  cumulative_words_per_character,
  cumulative_words_per_character$character %in% unique(most_words_per_episode$character)
)
cumulative_words_per_character = cumulative_words_per_character[order(-cumulative_words_per_character$x),]

most_words_per_episode$count = 1
character_num_episodes_most_talkative = aggregate(
  most_words_per_episode$count,
  by = list(
    character = most_words_per_episode$character
  ),
  sum
)
character_num_episodes_most_talkative = character_num_episodes_most_talkative[
  order(-character_num_episodes_most_talkative$x), ]
#--------------------------------------
# Plot
#--------------------------------------

ggplot(
  most_words_per_episode, 
  aes(
    x = factor(episode, levels = sort(unique(episode))), 
    y = factor(season, levels = rev(sort(unique(season)))), 
    fill = factor(character, levels = character_num_episodes_most_talkative$character)
  )
) + 
  geom_tile(color = "white", alpha = 0.8) + 
  geom_text(
    aes(label = paste(trimws(character, which = "both"), words_spoken, sep = "\n")), 
    size = 2,
    family = "mono"
  ) +
  coord_equal(ratio = 1) + 
  labs(
    x = "Episode",
    y = "Season",
    title = "Who is the Most Talkative Friend?",
    subtitle = "The character who talks the most in each episode, by word count",
    # fill = "Friend (num episodes as most talkative)",
    fill = "",
    caption = ""
  ) + 
  scale_fill_manual(
    values = brewer.pal(
      n = length(character_num_episodes_most_talkative$character), 
      name = "Accent"
    ),
    breaks = character_num_episodes_most_talkative$character,
    labels = paste(
      character_num_episodes_most_talkative$character,
      " (",
      character_num_episodes_most_talkative$x,
      ")",
      sep = ""
    )
  ) + 
  theme_economist() +
  theme(
    text = element_text(family = "mono"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    axis.ticks = element_blank(),
    axis.line.x = element_line(),
    axis.line.y = element_line(),
    plot.title = element_text(size = rel(2), margin = margin(t = 20, r = 0, b = 10, l = 0)),
    plot.subtitle = element_text(hjust = 0, margin = margin(t = 0, r = 0, b = 20, l = 0)),
    axis.title.x = element_text(face = "bold", margin = margin(t = 20, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(face = "bold", margin = margin(t = 0, r = 20, b = 0, l = 0)),
    legend.margin = margin(t = 0, r = 0, b = 10, l = 0)
  )

ggsave(
  paste("plot.png", sep = ""),
  path = "~/Developer/friends-dialog",
  dpi = 320,
  width = 12,
  height = 8,
  device = "png",
  units = "in"
)

#--------------------------------------
# Plot 2
#--------------------------------------

main_characters = c(
  "Ross",
  "Rachel",
  "Joey",
  "Phoebe",
  "Chandler",
  "Monica"
)

main_characters_and_other = c(
  main_characters, 
  "Other"
)

words_per_character_per_episode$label = ifelse(
  words_per_character_per_episode$character %in% main_characters,
  words_per_character_per_episode$character,
  "Other"
)

words_per_character_per_episode$label = factor(
  words_per_character_per_episode$label, 
  labels = main_characters_and_other
)

words_per_label_per_episode = aggregate(
  words_per_character_per_episode$x,
  by = list(
    label = words_per_character_per_episode$label,
    season = words_per_character_per_episode$season,
    episode = words_per_character_per_episode$episode
  ),
  sum
)

words_per_label_per_episode$label = factor(
  words_per_label_per_episode$label, 
  labels = main_characters_and_other
)

words_per_label_per_episode$season = paste(
  "Season ",
  words_per_label_per_episode$season,
  sep = ""
)

words_per_label_per_episode$season = factor(
  words_per_label_per_episode$season,
  levels = str_sort(unique(words_per_label_per_episode$season), numeric = TRUE)
)

words_per_label_per_episode$episode = paste(
  "Episode ",
  words_per_label_per_episode$episode,
  sep = ""
)

words_per_label_per_episode$episode = factor(
  words_per_label_per_episode$episode,
  levels = str_sort(unique(words_per_label_per_episode$episode), numeric = TRUE)
)

ggplot(
  words_per_label_per_episode,
  aes(
    area = x,
    label = label,
    fill = label
  )
) + 
  geom_treemap(
    
  ) +
  geom_treemap_text(
    family = "mono",
    color = "black",
    place = "center"
  ) + 
  facet_grid(
    episode ~ season,
    switch = "y", 
    space = "free", 
  ) +
  coord_equal(
    
  ) +
  labs(
    title = "The One with the Talkative Friends",
    subtitle = "Proportion of words spoken by each character, in each episode.",
    caption = "",
    fill = ""
  ) + 
  scale_fill_manual(
    values = brewer.pal(
      n = length(main_characters_and_other), 
      name = "Accent"
    ),
    breaks = main_characters_and_other,
    labels = main_characters_and_other
  ) + 
  theme(
    text = element_text(family = "mono"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    axis.title = element_blank(),
    strip.text.y.left = element_text(angle = 0),
    strip.text = element_text(size = rel(1), face = "bold"),
    plot.background = element_rect(fill="aliceblue", color = NA),
    plot.title = element_text(size = rel(3), face = "bold", margin = margin(b = 10)),
    plot.subtitle = element_text(size = rel(1.5), hjust = 0, margin = margin(t = 0, r = 0, b = 20, l = 0)),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    legend.margin = margin(t = 0, r = 0, b = 0, l = 10),
    legend.background = element_rect(fill="aliceblue", color = NA),
    legend.text = element_text(size = rel(1.5), face = "bold")
  )

ggsave(
  paste("plot_proportion.png", sep = ""),
  path = "~/Developer/friends-dialog",
  dpi = 320,
  width = 16,
  height = 24,
  device = "png",
  units = "in"
)

