---
title: "The Book of Mormon"
author: "David Yang"
date: "04 13, 2021"
output:
  html_document:  
    keep_md: true
    toc: true
    toc_float: true
    code_folding: hide
    fig_height: 6
    fig_width: 12
    fig_align: 'center'
---






```r
dat<- read_rds(gzcon(url("https://byuistats.github.io/M335/data/BoM_SaviorNames.rds")))
dat1 <- rio::import("http://scriptures.nephi.org/downloads/lds-scriptures.csv.zip") %>%
  filter(volume_id == 3)
```

## Background

In 1978 Susan Easton Black penned an article in the Ensign title Even statistically, he is the dominant figure of the Book of Mormon. which makes some statistical claims about the Book of Mormon. With our “string” skills we are going to check her result and build an improved statistic using using number of words between references.

## Data Wrangling


```r
datnames <- str_c(dat$name, collapse = "|")
wcount <- function(list){slist = list %>% 
  unlist(); count = stri_count_words(slist); count;}
New_dat <- dat1 %>% 
  mutate(text = str_split(scripture_text, datnames), 
         reference = str_extract_all(scripture_text, datnames)) %>% 
  unnest(text) %>% 
  mutate(same_verse = ifelse(verse_title == lead(verse_title), T, F),
         group = cumsum(same_verse),
         group = ifelse(!same_verse,  group + 1, group)) %>% 
  group_by(group) %>% 
  mutate(text = str_c(text, collapse = "|")) %>%
  distinct(group, .keep_all = TRUE) %>% 
  mutate(word_count = as.numeric(map(text,wcount))) %>% 
  ungroup() %>% 
  mutate(verse_end = lead(verse_number),
         chapter_end = lead(chapter_number),
         book_end = lead(book_short_title)) %>% 
  select(text, reference, book_short_title, chapter_number, verse_number, book_end, 
         chapter_end, verse_end, group, word_count) %>% 
  rename(book_start = book_short_title, 
         chapter_start = chapter_number, 
         verse_start = verse_number)
New_dat$book_start <- factor(New_dat$book_start)
```

## Data Visualization


```r
table1 <- New_dat %>%  
  group_by(book_start)  %>% 
  summarise(Count = n()) %>% 
  arrange(-Count) %>% 
  rename(Book = book_start) 
knitr::kable(table1) 
```



|Book   | Count|
|:------|-----:|
|Alma   |  1020|
|2 Ne.  |   595|
|1 Ne.  |   464|
|Mosiah |   460|
|3 Ne.  |   455|
|Ether  |   228|
|Hel.   |   209|
|Morm.  |   193|
|Moro.  |   184|
|Jacob  |   153|
|4 Ne.  |    43|
|Enos   |    23|
|Omni   |    21|
|W of M |    15|
|Jarom  |     8|



```r
New_dat %>% 
  group_by(book_start) %>% 
  ggplot(aes(x = fct_reorder(book_start, word_count), y = word_count)) +
  geom_violin(alpha = 0.2, outlier.colour = "red") +
  geom_hline(aes(yintercept = mean(word_count))) + 
  scale_y_continuous(limits = c(0,250)) +
  coord_flip() %>% 
  labs(title = "Numbers of Savior name in the Book of Mormon", 
       x = "", 
       y = "Savior's name Count", 
       color = "Average word count") +
  theme_bw()
```

![](The-Book-Of-Mormon_files/figure-html/unnamed-chunk-2-1.png)<!-- -->
