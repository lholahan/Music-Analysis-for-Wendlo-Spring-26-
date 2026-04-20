# Music-Analysis-for-Wendlo-Spring-26-
🎵 Wendlo Music Popularity Analysis

📌 Project Overview

This project analyzes what drives song popularity in Wendlo’s music catalog using Spotify audio features, lyric emotion analysis, and statistical modeling.

The goal is to identify which characteristics (musical and emotional) are most strongly associated with listener engagement.

📊 What This Project Includes
Analysis of 24 Wendlo songs using Spotify audio features (BPM, Energy, Happiness, etc.)
Lyric emotion analysis using the NRC Emotion Lexicon
Statistical modeling in R to identify key predictors of popularity
Validation testing on other artists to test generalizability
An interactive HTML report with charts and explanations

🎯 Key Findings
Happiness Drives Popularity
Happiness is the only statistically significant predictor of popularity (p = 0.038)
Songs that feel more uplifting and positive tend to perform better
Other Features Are Less Important
BPM and Liveness are not statistically significant
Emotional tone matters more than technical features
Unique Artistic Signature
The model does not generalize well to other artists
This suggests Wendlo’s success is tied to a distinct, personal style

🚀 How to Use This Project (No Technical Experience Needed)
👀 Option 1: View the Report (Recommended)
Find the file:
detailed report on Wendlo's popularity.html
Open it:
Double-click the file
Or right-click → “Open with” → choose a web browser (Chrome, Safari, Edge)
Explore:
Scroll through the report
Hover over charts for details
Read explanations written in plain English

👉 This is the main way to use the project

📊 Option 2: Understand the Results

Inside the report, focus on:

“Most Important Finding” → main takeaway
“Evidence-Based Summary” → conclusions
“What This Means in Plain English” → simplified explanation

🧠 Option 3: Run the Analysis Yourself (Optional)

If you want to reproduce the results:

Install:
R → https://cran.r-project.org/
RStudio → https://posit.co/download/rstudio-desktop/
Open the file:
statistical testing for Wendlo on popularity.R
Click Run (or press Ctrl + Enter)

👉 This will rerun all statistical tests used in the project

📁 File Guide
detailed report on Wendlo's popularity.html
→ Interactive report with visuals and explanations (start here)
statistical testing for Wendlo on popularity.R
→ Contains all statistical models and testing code

⚠️ Limitations
Results show correlation, not causation
Based on a small dataset (24 songs)
Does not include:
Audience demographics
Marketing or release strategy

💡 Practical Takeaways
Emotional tone—especially uplifting “happiness”—matters most
Focus on authentic storytelling and feeling
Use insights as guidance, not strict rules

🛠️ Tools Used
R (statistical modeling)
NRC Emotion Lexicon (text analysis)
Spotify API audio features
Plotly (interactive charts)
HTML/JavaScript
