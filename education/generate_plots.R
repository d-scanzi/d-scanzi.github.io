library(tidyverse)
library(janitor)
library(here)
library(wordcloud2)
library(kableExtra)
library(ggwaffle)
library(glue)
library(emojifont) 

## LOAD AND SET-UP
raw_feedback <- read_csv(here("feedback_tables", "feedback_collected.csv"))
questions    <- colnames(raw_feedback)

raw_feedback <- raw_feedback %>% 
    pivot_longer(cols = everything(),
                 names_to = "question",
                 values_to = "answers")
## WORD CLOUD
descriptors <- raw_feedback %>% 
    filter(question %in% questions[5:9]) %>% 
    mutate(answers = tolower(answers)) %>% 
    drop_na() %>% 
    group_by(answers) %>% 
    summarise(freq = n()) %>% 
    rename(word = answers)

# Requires manual saving
word_cloud_plot <- wordcloud2(descriptors, minRotation = -pi/2, maxRotation = -pi/2, backgroundColor = '#000000')

## ABOVE/BELOW/AVERAGE
question_1_percentages <- raw_feedback %>% 
    filter(question == questions[1]) %>% 
    replace_na(list(answers="Unanswered")) %>% 
    table() %>% 
    prop.table()

students_grades <- raw_feedback %>% 
    filter(question == questions[1]) %>% 
    replace_na(list(answers= "Unanswered")) %>% 
    arrange(answers)

students_grades$x <- rep(1:10, 9)
students_grades$y <- rep(1:9, each = 10)
students_grades$label <- fontawesome("fa-graduation-cap")


marks_plot <- ggplot(students_grades, aes(x, y, colour = answers)) + 
    geom_text(aes(label=label), family='fontawesome-webfont', size=4, key_glyph = "rect") +
    scale_colour_manual(values = c("#ffffff", "#808080" , "gray"),
                        labels = c(glue("Above Average\n({round(question_1_percentages[1]*100)}%)"),
                                   glue("Average\n({round(question_1_percentages[2]*100)}%)"),
                                   glue("Not Responded\n({round(question_1_percentages[3]*100)}%)"))) +
    labs(fill = "",
         title = "How would your rate my tutorials compared to other tutorials/classes?",
         x = "",
         y = "") +
    coord_equal() +
    theme_waffle() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          legend.background = element_rect(fill="transparent", color=NA),
          panel.background = element_rect(fill="#000000", color=NA),
          plot.background  = element_rect(fill="#000000", color=NA),
          legend.text = element_text(colour = "#ffffff"))

ggsave("grades_from_students.png", marks_plot)

## PROPORTIONS OF GRADES
question_2_percentages <- raw_feedback %>% 
    filter(question == questions[2]) %>% 
    replace_na(list(answers="Unanswered")) %>% 
    table() %>% 
    prop.table() %>% 
    as_tibble()
question_2_percentages$answers <- factor(question_2_percentages$answers, levels = c("A+", "A", "A-", "B+", "B", "B-", "Unanswered"))

grade_plot <- question_2_percentages %>% 
    ggplot(aes(y=question, x=n, fill=answers)) +
    geom_col(position = "fill", width = .15) +
    geom_text(aes(label = paste0(round(n * 100), "%")),
              position = position_fill(vjust = 0.5),
              size = 4) +
    labs(fill  ="",
         title = "What grade do you think I should recieve as a tutor?") +
    #scale_fill_brewer(palette="BuGn", direction = -1) +
    scale_fill_grey(start="#1f1f1f", end="#ffffff") +
    guides(fill = guide_legend(nrow = 1, reverse = T)) +
    theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        legend.background = element_rect(fill="transparent", color=NA),
        panel.background = element_rect(fill="#000000", color=NA),
        plot.background  = element_rect(fill="#000000", color=NA),
        legend.position = "bottom",
        plot.title = element_text(hjust = .5)
    ) +
    scale_y_discrete(expand = expansion(mult = .1)) +
    scale_x_continuous(labels = scales::percent)

ggsave("grades_from_students.png", grade_plot)
