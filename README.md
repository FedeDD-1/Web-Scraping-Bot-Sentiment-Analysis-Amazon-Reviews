# Sentiment Analysis of Amazon Reviews for the Tapo C210 Camera  
### A Comparison Between Lexical and Contextual Methods  

**Authors:**  
- Giovanni Alfarano  
- Federico Durante  
- Dario Perico  

---

## Introduction  
This project analyzes the sentiment of Amazon UK reviews related to the product **“Tapo C210 2K 3MP”**, a home surveillance camera.  
The objective is to identify strengths, weaknesses, and the overall user experience by processing a corpus of reviews.  

We collected the data through a **web scraping algorithm in R**, applying filters for ratings and review sorting.  
Two sentiment analysis approaches were compared:  

1. **Bing Lexicon Method** – a classic lexicon-based approach assigning binary polarity to words.  
2. **UDPipe Enhanced Method** – a contextual method that accounts for **negators, amplifiers, and deamplifiers**, providing more refined sentiment scores.  

The results of the two methods were compared with user-assigned star ratings to evaluate consistency and insights.  

---

## Data  
Amazon limits scraping by showing a maximum of **500 reviews per product**, divided into **100 reviews per star rating** (from 1 to 5). By changing sorting options (e.g., “top reviews” vs. “most recent”), additional reviews can be accessed, though with overlap.  

- **Total collected reviews:** 1,000  
- **Duplicates removed:** 420  
- **Final dataset size:** 572 unique reviews  
- **Processing:** Non-English reviews were automatically translated into English.  

**Key dataset characteristics:**  
- Average length: **71 words** (median: 38, st.dev: 96)  
- Min: 1 word | Max: 921 words  
- Longer reviews typically correspond to **2–4 star ratings**, while extreme ratings (1 or 5 stars) tend to be shorter.  

>  Note: Due to stratified sampling by star level, the dataset’s average rating is **2.98**, lower than Amazon’s official global rating of **4.6/5**. This distortion is acceptable since the study focuses on **patterns**, not absolute averages.  

---

## Results and Discussion  

### Correlations with star ratings  
- **Bing sentiment scores vs. stars:** 0.39  
- **UDPipe sentiment scores vs. stars:** 0.46  

Both methods capture the general sentiment direction, with a **mutual correlation of 0.85** between them.  

### Insights  
- **Bing** underperforms in negative reviews (1–3 stars) due to its inability to capture negations, amplifiers, and sarcasm.  
- **UDPipe** provides scores closer to expected distributions thanks to contextual adjustments.  
- Positive words: *good, easy, quality, great* → appear in 4–5 star reviews.  
- Negative words: *no, problem, issue, disconnect* → dominate in 1–2 star reviews.  
- Frequent bigrams reveal key themes:  
  - **Memory management:** *sd card, micro sd, memory card*  
  - **Camera performance:** *night vision, picture quality, image quality*  
  - **Brand/app:** *tapo app, tp link* (with *tp link* appearing more often in negative reviews).  

### Sentiment distribution vs. star ratings  
Using classification thresholds for UDPipe (score < -1 negative, > 1 positive, else neutral), and resampling 100 times to mirror Amazon’s real distribution:  
- Negative: **85%** (Amazon: 92%)  
- Neutral: **6%** (Amazon: 4%)  
- Positive: **8%** (Amazon: 4%)  

---

## Conclusions  
- Lexicon-based methods show **limited predictive accuracy**, with UDPipe only reaching a correlation of 0.46 with star ratings.  
- A BERT-based classifier tested on the dataset achieved **31% accuracy**, confirming the **difficulty of predicting star ratings from free-form text**.  
- Despite predictive limitations, the analysis identified **clear strengths and weaknesses**:  
  - **Strengths:** ease of use, night vision quality  
  - **Weaknesses:** connection issues, memory card compatibility, occasional image quality concerns  

This demonstrates that even relatively simple text mining techniques can provide **useful insights** with **low computational cost**, compared to more complex models such as Large Language Models.  

---

## Repository Structure  
- `data/` → cleaned dataset of reviews  
- `scripts/` → R scripts for scraping, preprocessing, and analysis  
- `results/` → visualizations and outputs from sentiment analysis  
- `report.pdf` → full project report with methodology and findings  

---

## Technologies Used  
- **R** for scraping, preprocessing, and analysis  
- **tidyverse** for data manipulation  
- **bing lexicon** for lexicon-based sentiment analysis  
- **udpipe** for contextual sentiment analysis  
- **Google Translate API** for automatic translation of non-English reviews  

---

