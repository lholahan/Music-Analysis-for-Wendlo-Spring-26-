library(shiny)
library(readxl)
library(car)
library(leaps)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(stringr)
library(tidytext)

options(contrasts = c("contr.sum", "contr.poly"))

# -------------------------------------------------------------------
# Brand styling and helper functions
# -------------------------------------------------------------------

brand_colors <- list(
  primary = "#C56B2F",
  secondary = "#6F3C8E",
  accent = "#6E9655",
  gold = "#D79A31",
  text = "#2B2330",
  background = "#FBF7F2",
  soft_orange = "#F8E4D4",
  soft_purple = "#E8DDF3",
  soft_green = "#E4F0DD",
  soft_blue = "#DFE9F7",
  border = "#E8DCCA"
)

make_insight_box <- function(technical, plain) {
  tags$div(
    class = "insight-box",
    
    tags$div(
      class = "insight-label technical",
      style = "color: white;",
      "Technical"
    ),
    
    tags$p(technical),
    
    tags$div(
      class = "insight-label plain",
      "Plain English"
    ),
    
    tags$p(plain)
  )
}

pretty_predictor <- function(x) {
  dplyr::recode(
    x,
    BPM = "Tempo (BPM)",
    Duration = "Track Length",
    Energy = "Energy",
    Danceability = "Danceability",
    Happiness = "Happiness",
    Acousticness = "Acousticness",
    Liveness = "Liveness",
    Spechiness = "Speechiness",
    Speechiness = "Speechiness",
    Loudness = "Loudness",
    .default = x
  )
}

data_path <- file.path(path.expand("~"), "Downloads", "Wendlo Song Data for r.xlsx")

lyrics_df <- data.frame(
  song = c(
    "Untethered", "Must Be Nice", "Wasting Time With You", "Shadow",
    "Broken Glass", "Downtown", "Sweet Sleep", "Just You & Me",
    "Soft Spot", "Seasick", "Love Me"
  ),
  release_date = as.Date(c(
    "2025-07-30",
    "2025-06-06",
    "2025-04-09",
    "2021-08-05",
    "2020-10-16",
    "2019-06-21",
    "2019-03-22",
    "2017-06-23",
    "2017-06-23",
    "2017-06-23",
    "2026-02-13"
  )),
  lyrics = c(
    "It's been radio silent, sorry. I'm barely used to my own sensibilities. I'm peeking over the fence line. I'm not sure your neat little life will fit me. Then you speak and I flinch. My heart beats like I'm failing a pop quiz. You reach over and I crumble in your hands. You pull me in. Back to center, then into orbit. When I'm in space among the nameless. You find where I came untethered.",
    "I'm not surprised by your cold shoulder. They should call you Mister Freeze. When you took my heart and iced it over. I couldn't believe you didn't say \"sorry\" at least. Should've known you wouldn't care. You're not that kind of guy. Thinking of somebody else. Has never crossed your mind. I just wonder what it's like. Must be nice. Taking the shortcut. And watching me run for my life. Must be nice. Getting royal treatment. And treating me like a waste of time. Must be nice. On top of your world. Head in the clouds. But don't look down. 'Cause if you do. You'll ruin the illusion. That it's always been about you. Sooner or later. You'll understand. Sooner or later. You're gonna get kicked out of La La Land. Sooner or later. It's gonna come to an end. Nothing lasts forever. So enjoy it while you can. When you get back to planet Earth. Don't try to reconnect. I'm happier just acting like. You don't even exist. I guess ignorance is bliss.",
    "I'm not an expert. At anything yet. I try to defend. How my life is spent. What in the world. Am I supposed to do. I'm so good at wasting time with you. I don't know a lot. About this or that. I've tried teaching myself. And taking a class. At the end of the day. What's there to prove. I'm so good at wasting time with you. We go for walks. And get lost on purpose. 'Cause time is a construct. That we're not concerned with. Deadlines, meetings. I've missed a few. I'm so good at wasting time with you. I try to search. What's in my heart. What my dreams are made of. But where do I start. If I had my way. All I would do. Is sit here and waste. Some more time with you. I'm so good at wasting time with you.",
    "Let me be your shadow. I'll stay in the lines. And when you move. I can follow right behind. I could stay for hours. I lose track of time. 'Cause your presence. Leaves me no presence of mind. Let me see you in motion. Capture all your steps. Feel every breath. Trace your figure around the edge. I can feel you hold back. This game of hard to get. When will it end. I'm desperate enough to beg. Got me working so hard for your love. Fascinating me, turning me on. Come here closer, I can't get enough. I get lost just thinking of us. Only mystery I'm trying to solve. Tunnel vision, you're all that I want. Let me feel your heavenly touch. Taking me up to a whole 'nother world. I watch your eyes wander. Take me where you go. I wanna know. Things only a lover knows. I can feel the distance. Even when you're close. If you want me, baby. You don't let it show. Got me working so hard for your love. Fascinating me, turning me on. Come here closer, I can't get enough. I get lost just thinking of us. Only mystery I'm trying to solve. Tunnel vision, you're all that I want. Let me feel your heavenly touch. Taking me up to a whole 'nother world. Let me be your shadow. Just let me be your shadow. Let me be your shadow. Just let me be your shadow. Got me working so hard for your love. Fascinating me turning me on. Come here closer, I can't get enough. I get lost just thinking of us. Only mystery I'm trying to solve. Tunnel vision you're all that I want. Let me feel your heavenly touch. Taking me up to a whole 'nother world. Let me be your shadow. Oh. Just let me be your shadow. Let me be your shadow. Just let me be your shadow. Let me be your shadow. Take me where you go. I wanna know. Things only a lover knows.",
    "Sorry for waking you up. With a broken glass. I woke up in a hurry. And I moved too fast. But if I get lucky. Maybe today will last. Into the weekend. We can work it all out. I need a reason. Not to twist and shout. Run up the walls. Until it all falls down. So it goes. Oh you're such a mess right now. But it beats waking up alone. Sorry for waking you up. With a broken glass. It's still my fault. I'm still moving too fast. I hope it's not something. You can't get past. This day's finally worked its way. Down under your skin. Down to the bone. Where The Itch begins. Are you still with me. 'Cause I'm on the edge. So it goes. We're broken as this shattered glass. But it beats waking up alone. 'Round and 'round we go. Oh you're such a mess right now. But it beats waking up alone. I've been repeating. I'm not bleeding this one out. I'm getting better. It bears repeating. I'm not bleeding this one out, but. If there's a hill you wanna die on. I'll meet you there. Ooh. Ooh. Ooh. Ooo. So it goes. We're broken as this shattered glass. But it beats waking up alone. 'Round and 'round we go. Oh you're such a mess right now. Oh you're such a mess right now. Oh we're such a mess right now. But it beats waking up alone.",
    "I used to know what you wanted. A pretty face and bright blue rooms. I'd have a seat on the staircase. You'd wash the dishes and hum this tune. Downtown. Where the dreams that we made. Are still up for debate. Downtown. Every scene at night coming. Straight from my mind. Downtown. Where the dreams that we made. Are still up for debate. Downtown. Every scene at night coming. Straight from my mind. (Mind, mind mind, mind). I've had this feeling for awhile. The kind you try but can't ignore. I know we said and we meant it. But then the love became a chore. Downtown. Where the dreams that we made. Are still up for debate. Downtown. Every scene at night coming. Straight from my mind. Downtown. Where the dreams that we made. Are still up for debate. Downtown. Every scene at night coming. Straight from my mind. (Mind, mind, mind, mind). (Life, away, away, life). (Life, life, life, life). When you're up at night. Are you thinking of me? While I'm bed. Planning your life away. When you're up at night. Are you thinking of me? While I'm in bed. Dreaming your life away. Downtown. Where the dreams that we made. Are still up for debate. Downtown. Every scene at night coming. Straight from my mind. Downtown. Where the dreams that we made. Are still up for debate. Downtown. Every scene at night coming. Straight from my mind.",
    "I couldn't escape myself. All alone with no one else. Surrounded by a thousand places. I have never seen. Sunk down below the bed I'm in. Can't get up until it ends. And when I wake it slips away. From my memory. If it's just a dream. How'd you talk to me and sway my feelings. If it's just me. Can't trust myself to know. If it's just a dream. When I wake I'll be alright. If it's real life. Sleep came just in time. Last night when I saw you, dear. Only took a moment. It was clear that you were far away. Somewhere out of reach. I had made the whole thing up. Worked my stomach into knots. Imagined what you'd say to me. If we were face to face. And not just in my dreams. If it's just a dream. How'd you talk to me and sway my feelings. If it's just me. Can't trust myself to know. If it's just a dream. When I wake I'll be alright. If it's real life. Sleep came just in time. Sweet sleep came just in time. Sweet sleep came just in time. I'm spinning it feels like in circles. My living room feels like a circus. I couldn't have finished the movie I'm living. This whole thing feels like a rehearsal. I tried to recite from the verses. I can't and the only thing worse is. I open my mouth hoping that I'm not dreaming. But nothing comes out—only curses. Nothing comes out—only curses. Can I reverse this? If it's just a dream … Sweet sleep came just in time. Sweet sleep came just in time. Sweet sleep came just in time. Sweet sleep came just in time.",
    "Why don't we talk this over. It's plain to see this isn't over. Just you and me. Taking cover for our lives. Rain or shine. We still have to weather. Make up our minds. Are we better together. This bitter wine. We've been tasting won't last. Last for long. Fast-forward life. Look into the future. Am I by your side. Are you holding me dearer. This precious time. We can't waste it, no. We'll never know. Ba ba da-da, da da da, da-da da. Ba ba da-da, da da da, da-da da. Ba ba da-da, da da da da, da-da da-da da-da. Ba ba da-da, da da da, da-da da. Rain or shine. We still have to weather. Make up our minds. Are we better together. This bitter wine. We've been tasting won't last. Last for long. Baby, it won't last for long. Maybe it won't last for long. Ooh.",
    "You found a soft spot on the floor. You been sleeping there all night. You been sleeping there all night. You found a soft spot in my heart. You been soaking up the spotlight. You been telling me it's right. You found a soft spot on the floor. You been sleeping there all night. You been sleeping there all night. Let's try to make the most of this. Try to figure out what happened. Before we reach the end. I tried to tell you to slow down. A little bit more. Or maybe just even out. Before the Lord. God as my witness, I resign. You got yours and I got mine. And I meant when I said. I'd hold out hope for you, babe. Whoa, and I am a train run off its track. Like beauty in the darkness. Wasted in the darkness. I had a dream you're coming back. And this whole thing never happened. And now you're telling me it's alright. Well I don't know. I tried to tell you to slow down. A little bit more. Or maybe just even out. Before the Lord. God as my witness, I resign. You got yours and I got mine. And I meant when I said. I'd hold out hope for you, babe.",
    "Crisscross, tiptoe. Lean back, let it go. I've been waiting for your guns. Swing low, shoot high. Breathe in deep, let out your sigh. Kid take it easy on me. I'm getting seasick in my dreams. I'm lost at sea without you next to me. What a cliché melody to hold the words you speak. When I can't make up my mind. Indecision will decide. Indecision will decide what to do with me. Black on white, clear as day. You're in my life, here to stay. Go ahead, tell me something new. Stop by, stay there. Speak your mind, clear the air. Sick of the motion in between. Sick of the motion in between. I'm lost at sea without you next to me. What a cliché melody to hold the words you speak. When I can't make up my mind. Indecision will decide. Indecision will decide. Indecision will decide. Will decide. I, I. I'm lost at sea without you next to me. What a cliché melody to hold the words you speak. When I can't make up my mind. Indecision will decide. Indecision will decide. Indecision will decide. I, I, I, I.",
    "Love me. 'Til I'm annoyed. While I still have my coat on. When I walk through the door. Greet me in the hall. Under fluorescent light. Love with a \"hello\". That lasts the whole night. Mmm. Love me. When I've gone astray. Stubborn, unyielding. 'Til I get my way. Offer up a touch. I soften at your hand. When it comes to you. I don't stand a chance. Mmm. What is it about you. That brings me to my knees. Why is it I need you. Overwhelmingly. Love me. Up to the end. When our bodies slow. And the light starts to bend. Even through the pain. And I know it'll hurt. Love me by waiting. Let me go first. Mmm"
  ),
  streams = c(
    32273, 237948, 219828, 164873, 336279,
    520395, 71438, 114096, 42348, 26906, 11889
  ),
  stringsAsFactors = FALSE
)

lyrics_release_order <- lyrics_df %>%
  arrange(release_date, song) %>%
  mutate(release_label = paste0(song, " (", format(release_date, "%m/%d/%y"), ")"))

# -------------------------------------------------------------------
# UI
# -------------------------------------------------------------------

ui <- fluidPage(
  tags$head(
    tags$style(HTML(sprintf("
      body {
        background-color: %s;
        color: %s;
      }
      .insight-box {
        background: linear-gradient(180deg, #ffffff 0%%, %s 100%%);
        border: 1px solid %s;
        border-left: 7px solid %s;
        border-radius: 18px;
        padding: 16px 18px;
        margin-bottom: 18px;
        box-shadow: 0 8px 22px rgba(43, 35, 48, 0.06);
      }
      .insight-box p {
        margin-bottom: 8px;
        line-height: 1.45;
        font-size: 14px;
      }
      .insight-label {
        display: inline-block;
        padding: 6px 12px;
        border-radius: 999px;
        font-weight: 700;
        font-size: 12px;
        letter-spacing: 0.2px;
        margin-bottom: 10px;
      }
      .insight-label.technical {
        background: %s;
        color: %s;
      }
      .insight-label.plain {
        background: %s;
        color: %s;
      }
      .section-title {
        color: %s;
        font-weight: 700;
        margin-top: 6px;
        margin-bottom: 10px;
      }
      .well {
        border-radius: 16px;
        border: 1px solid #E5DDD2;
        box-shadow: 0 4px 14px rgba(0,0,0,0.04);
      }
    ",
                            brand_colors$background,
                            brand_colors$text,
                            brand_colors$soft_orange,
                            brand_colors$border,
                            brand_colors$secondary,
                            brand_colors$secondary,
                            brand_colors$secondary,
                            brand_colors$soft_green,
                            brand_colors$text,
                            brand_colors$text
    )))
  ),
  
  titlePanel("Wendlo Song Analytics Dashboard"),
  
  tabsetPanel(
    tabPanel(
      "Data Preview",
      h3(class = "section-title", "Dataset Snapshot"),
      make_insight_box(
        "This tab shows the working data source used across the dashboard. The app reads the Excel file, standardizes the speechiness field name when needed, and uses the same cleaned structure for modeling.",
        "This is the raw material the rest of the dashboard is built from. It lets us confirm the file loaded correctly before interpreting the models."
      ),
      DT::dataTableOutput("preview")      
    ),
    
    tabPanel(
      "Multicollinearity Check (VIF)",
      h3(class = "section-title", "Variance Inflation Factors"),
      make_insight_box(
        "Variance Inflation Factor quantifies multicollinearity among predictors in the full regression model. Values near 1 suggest little overlap, while larger values indicate that some predictors may be carrying similar information.",
        "This tells us whether the song features are too similar to each other. If they are, the model may have a harder time separating their individual effects."
      ),
      plotlyOutput("vif_plot", height = "500px"),      
      br(),
      tableOutput("vif_table")
    ),
    
    tabPanel(
      "Model Selection (Best Subsets)",
      h3(class = "section-title", "Best Subsets Search"),
      make_insight_box(
        "Best subsets regression compares multiple predictor combinations and summarizes fit statistics such as adjusted R-squared, Mallows' Cp, BIC, and SSE.",
        "This helps us see which combination of song features gives the strongest explanation of popularity without making the model unnecessarily complicated."
      ),
      tableOutput("subsets_table")
    ),
    
    tabPanel(
      "Model Comparison (Nested ANOVA)",
      h3(class = "section-title", "Nested Model Test"),
      make_insight_box(
        "This nested ANOVA compares a smaller model with a larger one to test whether the added predictors improve fit enough to justify the extra complexity.",
        "In plain terms, this asks whether the bigger model actually earns its keep or just adds noise."
      ),
      tableOutput("nested_anova")
    ),
    
    tabPanel(
      "Type III ANOVA",
      h3(class = "section-title", "Type III Tests"),
      make_insight_box(
        "Type III ANOVA evaluates each predictor while adjusting for the other terms already in the model. With sum-to-zero contrasts, the tests reflect each predictor's unique contribution given the full set of included variables.",
        "This helps us see whether each song feature still matters after we account for the other features in the model."
      ),
      tableOutput("type3_anova")
    ),
    
    tabPanel(
      "Diagnostic Plots",
      h3(class = "section-title", "Model Diagnostics"),
      make_insight_box(
        "These plots assess linearity and residual behavior for the core regression model by comparing residuals against fitted values and the main predictors.",
        "This is our check that the model is behaving sensibly and not missing a pattern it should have captured."
      ),
      fluidRow(
        column(
          4,
          selectInput(
            "resid_focus",
            "Highlight predictor",
            choices = c("Fitted Values", "Tempo (BPM)", "Happiness", "Liveness"),
            selected = "Fitted Values"
          )
        ),
        column(
          4,
          sliderInput(
            "resid_alpha",
            "Point transparency",
            min = 0.2, max = 1,
            value = 0.8,
            step = 0.1
          )
        ),
        column(
          4,
          checkboxInput(
            "resid_smooth",
            "Show smooth trend line",
            value = TRUE
          )
        )
      ),
      plotlyOutput("resid_plots", height = "650px")
    ),
    
    tabPanel(
      "Quadratic Diagnostics",
      h3(class = "section-title", "Curvature Check"),
      make_insight_box(
        "This section adds centered quadratic terms for BPM and Liveness to test whether the relationship with popularity is curved rather than purely linear.",
        "This helps us see whether the effect of tempo or liveness changes at very low or very high values."
      ),
      fluidRow(
        column(
          4,
          selectInput(
            "quad_focus",
            "Highlight predictor",
            choices = c("Fitted Values", "Tempo (BPM)", "Happiness", "Liveness"),
            selected = "Fitted Values"
          )
        ),
        column(
          4,
          sliderInput(
            "quad_alpha",
            "Point transparency",
            min = 0.2, max = 1,
            value = 0.8,
            step = 0.1
          )
        ),
        column(
          4,
          checkboxInput(
            "quad_smooth",
            "Show smooth trend line",
            value = TRUE
          )
        )
      ),
      tableOutput("quad_anova"),
      plotlyOutput("quad_resid_plots", height = "650px")
    ),
    
    tabPanel(
      "Lyric Emotion Streamgraph",
      h3(class = "section-title", "Emotion Flow Across Songs"),
      make_insight_box(
        "This streamgraph uses NRC sentiment counts to show how the emotional profile changes across songs. The songs are ordered chronologically by release date.",
        "This gives us a visual of how the mood of the lyrics evolves over time."
      ),
      plotlyOutput("sentiment_plot", height = "700px"),
    ),
    
    tabPanel(
      "Emotion Pair Synergy",
      h3(class = "section-title", "Emotion Pair Interaction Models"),
      make_insight_box(
        "This section fits interaction models for every pair of emotion counts and compares how well each pair relates to streams. Adjusted R-squared, the interaction p-value, and AIC are used together to rank the pairs.",
        "This helps us see which combinations of emotions seem to work best together when explaining song popularity."
      ),
      plotlyOutput("emotion_pair_plot", height = "650px"),
      br(),
      tableOutput("emotion_pair_table"),
    ),
    
    tabPanel(
      "Model Validation",
      h3(class = "section-title", "Validation on the Second Artist Dataset"),
      make_insight_box(
        "This tab applies the trained Wendlo model to the May Erlewine and Jackie Evans sample to see whether the same pattern generalizes to another artist set.",
        "This is a reality check. It shows whether the model performs reasonably well on a different group of songs or whether the fit is more specific to Wendlo."
      ),
      tableOutput("may_anova"),
      tableOutput("may_results"),
      plotlyOutput("may_plot", height = "450px"),
    ),
    
    tabPanel(
      "Pleasantness-Arousal",
      h3(class = "section-title", "Emotion Space Map"),
      make_insight_box(
        "This scatterplot maps Happiness to Pleasantness and Energy to Arousal. It compares Wendlo songs with the validation dataset using a two-dimensional emotional framework.",
        "This shows which songs feel calmer, happier, or more energetic, and makes the emotional differences easy to compare at a glance."
      ),
      fluidRow(
        column(
          4,
          selectInput(
            "artist_filter",
            "Artist",
            choices = c("All", "Wendlo", "May Erlewine", "Jackie Evans"),
            selected = "All"
          )
        ),
        column(
          4,
          sliderInput(
            "pleasantness_filter",
            "Pleasantness range",
            min = 0, max = 100,
            value = c(0, 100),
            step = 1
          )
        ),
        column(
          4,
          sliderInput(
            "arousal_filter",
            "Arousal range",
            min = 0, max = 100,
            value = c(0, 100),
            step = 1
          )
        )
      ),
      plotlyOutput("va_plot", height = "700px"),
    )
  )
)
server <- function(input, output, session) {
  
  spotify <- reactive({
    data.frame(
      Song_Title = c(
        "Sweet Child O' Mine","Must Be Nice!","You Make My Dreams (Come True)","Love Me","September",
        "Broken Glass","Just you & Me","Wasting Time With You","Untethered","Sweet Sleep","Shadow",
        "White Christmas","Mele Kalikimaka","Soft Spot","Santa Baby","Let It Snow","Seasick",
        "For Now","Wild","Christmas Time is Here","Downtown","Never Going to Give You Up",
        "I Love You Always Forever","Spenard"),
      
      Popularity = c(30,37,33,26,24,17,17,34,19,10,11,2,2,4,2,2,3,3,3,2,0,20,19,0),
      
      BPM = c(118,156,182,132,117,172,120,82,117,126,150,66,115,78,113,116,140,124,128,58,90,95,95,125),
      
      Duration = c(119,209,136,135,158,179,245,151,187,251,234,187,146,232,146,166,203,270,240,246,181,170,245,247),
      
      Energy = c(23,43,39,11,18,17,14,35,48,41,48,23,24,34,28,27,27,10,28,18,31,21,19,32),
      
      Danceability = c(79,80,78,72,86,50,72,82,53,39,71,29,79,61,82,70,73,54,43,44,76,71,50,56),
      
      Happiness = c(42,78,97,38,49,32,13,61,50,14,53,4,73,16,78,36,13,7,8,5,47,64,24,50),
      
      Acousticness = c(73,88,14,95,89,17,81,80,58,65,16,61,92,49,93,72,62,80,75,69,51,93,92,67),
      
      Liveness = c(9,10,17,11,11,11,10,10,15,12,25,10,32,14,15,11,33,11,10,11,10,10,11,12),
      
      Speechiness = c(4,20,6,6,5,8,4,5,15,3,4,3,4,4,4,4,6,4,4,3,13,3,3,4),
      
      Loudness = c(-11,-7,-10,-16,-13,-7,-13,-7,-7,-9,-10,-12,-11,-10,-11,-12,-13,-14,-10,-13,-14,-11,-12,-8)
    )
  })
  
  train_dat <- reactive({
    dat <- spotify()
    set.seed(301)
    id <- sample(seq_len(nrow(dat)), floor(0.8 * nrow(dat)), replace = FALSE)
    dat[id, ]
  })
  
  may_data <- reactive({
    data.frame(
      Title = c(
        "Days Go By",
        "Palm of My Hand",
        "America's Daughter",
        "What the World Needs Now",
        "Anyway",
        "Sleep for Days",
        "Seven Brides",
        "Most of the Time (Live at The Bunker Studio)",
        "Growing Old",
        "Loving You Always (Live at The Bunker Studio)"
      ),
      Artist = c(
        "May Erlewine",
        "May Erlewine",
        "May Erlewine",
        "May Erlewine",
        "May Erlewine",
        "Jackie Evans",
        "Jackie Evans",
        "Jackie Evans",
        "Jackie Evans",
        "Jackie Evans"
      ),
      BPM = c(101, 79, 112, 191, 87, 80, 181, 140, 136, 85),
      Duration = c(179, 232, 247, 231, 205, 173, 179, 206, 210, 143),
      Popularity = c(48, 37, 34, 34, 33, 46, 46, 42, 37, 37),
      Energy = c(40, 33, 17, 8, 57, 55, 50, 16, 23, 6),
      Danceability = c(72, 53, 77, 43, 66, 66, 41, 49, 29, 42),
      Happiness = c(82, 46, 20, 36, 39, 68, 46, 32, 11, 23),
      Acousticness = c(20, 53, 90, 99, 0, 45, 88, 94, 95, 97),
      Liveness = c(15, 1, 8, 11, 9, 14, 12, 11, 9, 12),
      Speechiness = c(3, 4, 6, 6, 4, 3, 3, 4, 3, 4),
      Loudness = c(-11, -12, -15, -14, -7, -6, -7, -12, -10, -14),
      stringsAsFactors = FALSE
    )
  })
  
  output$preview <- DT::renderDataTable({
    spotify()
  }, options = list(
    pageLength = 10,
    scrollX = TRUE
  ))
  
  output$vif_plot <- renderPlotly({
    full_model <- lm(
      Popularity ~ BPM + Duration + Energy + Danceability + Happiness +
        Acousticness + Liveness + Speechiness + Loudness,
      data = train_dat()
    )
    
    vif_values <- car::vif(full_model)
    
    vif_df <- data.frame(
      Predictor = c(
        "Tempo (BPM)", "Track Length", "Energy", "Danceability", "Happiness",
        "Acousticness", "Liveness", "Speechiness", "Loudness"
      ),
      VIF = as.numeric(vif_values)
    )
    
    plot_ly(
      vif_df,
      x = ~Predictor,
      y = ~VIF,
      type = "bar",
      marker = list(
        color = c(
          brand_colors$primary, brand_colors$secondary, brand_colors$accent,
          brand_colors$gold, brand_colors$primary, brand_colors$secondary,
          brand_colors$accent, brand_colors$gold, brand_colors$primary
        )
      ),
      hovertemplate = "Predictor: %{x}<br>VIF: %{y:.2f}<extra></extra>"
    ) %>%
      layout(
        title = list(text = "VIF by Predictor"),
        xaxis = list(title = "Predictor", tickangle = -35),
        yaxis = list(title = "VIF", range = c(0, max(vif_df$VIF, na.rm = TRUE) + 1)),
        shapes = list(
          list(
            type = "line",
            x0 = -0.5,
            x1 = 8.5,
            y0 = 5,
            y1 = 5,
            line = list(color = "red", dash = "dash", width = 2)
          )
        )
      )
  })
  
  output$subsets_table <- renderTable({
    sub_fit <- regsubsets(
      Popularity ~ BPM + Duration + Energy + Danceability + Happiness +
        Acousticness + Liveness + Speechiness + Loudness,
      data = train_dat(),
      nvmax = 10
    )
    
    sub_sum <- summary(sub_fit)
    
    data.frame(
      Predictors = apply(sub_sum$which[, -1, drop = FALSE], 1, function(x) {
        paste(pretty_predictor(names(x)[x]), collapse = ", ")
      }),
      Adjusted_R2 = round(sub_sum$adjr2, 4),
      Cp = round(sub_sum$cp, 3),
      BIC = round(sub_sum$bic, 3),
      SSE = round(sub_sum$rss, 1)
    )
  })
  
  nested_anova_tbl <- reactive({
    model_small <- lm(
      Popularity ~ BPM + Happiness + Liveness,
      data = train_dat()
    )
    
    model_large <- lm(
      Popularity ~ BPM + Happiness + Liveness + Danceability + Loudness,
      data = train_dat()
    )
    
    anova_res <- anova(model_small, model_large)
    df <- as.data.frame(anova_res)
    df$Model <- c(
      "Tempo + Happiness + Liveness",
      "Tempo + Happiness + Liveness + Danceability + Loudness"
    )
    df <- df[, c("Model", setdiff(names(df), "Model"))]
    df
  })
  
  output$nested_anova <- renderTable({
    nested_anova_tbl()
  }, rownames = FALSE)
  
  type3_anova_tbl <- reactive({
    model <- lm(
      Popularity ~ BPM + Happiness + Liveness,
      data = train_dat()
    )
    
    a3 <- car::Anova(model, type = 3)
    df <- as.data.frame(a3, stringsAsFactors = FALSE)
    df$Predictor <- pretty_predictor(rownames(df))
    df <- df[, c("Predictor", setdiff(names(df), "Predictor"))]
    rownames(df) <- NULL
    df
  })
  
  output$type3_anova <- renderTable({
    type3_anova_tbl()
  }, rownames = FALSE)
  
  model1 <- reactive({
    lm(
      Popularity ~ BPM + Duration + Danceability + Happiness +
        Acousticness + Speechiness + Loudness,
      data = train_dat()
    )
  })
  
  output$resid_plots <- renderPlotly({
    m <- model1()
    dat <- train_dat()
    res <- resid(m)
    fit <- fitted(m)
    
    focus_var <- switch(
      input$resid_focus,
      "Fitted Values" = fit,
      "Tempo (BPM)" = dat$BPM,
      "Happiness" = dat$Happiness,
      "Liveness" = dat$Liveness
    )
    
    df <- data.frame(
      x = focus_var,
      res = res,
      fit = fit,
      label = paste0(
        "Fitted: ", round(fit, 2),
        "<br>Residual: ", round(res, 2),
        "<br>Tempo (BPM): ", dat$BPM,
        "<br>Happiness: ", dat$Happiness,
        "<br>Liveness: ", dat$Liveness
      )
    )
    
    p <- plot_ly(
      df,
      x = ~x,
      y = ~res,
      type = "scatter",
      mode = "markers",
      marker = list(
        size = 9,
        opacity = input$resid_alpha,
        color = brand_colors$secondary,
        line = list(color = "white", width = 1)
      ),
      text = ~label,
      hovertemplate = "%{text}<extra></extra>"
    ) %>%
      layout(
        title = list(text = "Interactive Residual Diagnostic"),
        xaxis = list(title = input$resid_focus),
        yaxis = list(title = "Residuals"),
        margin = list(l = 70, r = 30, t = 70, b = 70)
      )
    
    if (isTRUE(input$resid_smooth)) {
      sm <- stats::lowess(df$x, df$res)
      smooth_df <- data.frame(x = sm$x, y = sm$y)
      
      p <- p %>%
        add_lines(
          data = smooth_df,
          x = ~x,
          y = ~y,
          inherit = FALSE,
          line = list(color = brand_colors$primary, width = 3),
          hoverinfo = "skip",
          name = "Smooth"
        )
    }
    
    p
  })
  
  quad_model <- reactive({
    dat <- train_dat()
    
    dat$BPM_c <- dat$BPM - mean(dat$BPM, na.rm = TRUE)
    dat$Liveness_c <- dat$Liveness - mean(dat$Liveness, na.rm = TRUE)
    
    lm(
      Popularity ~ BPM_c + I(BPM_c^2) + Happiness + Liveness_c + I(Liveness_c^2),
      data = dat
    )
  })
  
  output$quad_anova <- renderTable({
    m <- quad_model()
    a3 <- car::Anova(m, type = 3)
    
    df <- as.data.frame(a3)
    df$Predictor <- rownames(df)
    df <- df[, c("Predictor", setdiff(names(df), "Predictor"))]
    rownames(df) <- NULL
    df
  }, rownames = FALSE)
  
  output$quad_resid_plots <- renderPlotly({
    m <- quad_model()
    dat <- train_dat()
    res <- resid(m)
    fit <- fitted(m)
    
    focus_var <- switch(
      input$quad_focus,
      "Fitted Values" = fit,
      "Tempo (BPM)" = dat$BPM,
      "Happiness" = dat$Happiness,
      "Liveness" = dat$Liveness
    )
    
    df <- data.frame(
      x = focus_var,
      res = res,
      fit = fit,
      label = paste0(
        "Fitted: ", round(fit, 2),
        "<br>Residual: ", round(res, 2),
        "<br>Tempo (BPM): ", dat$BPM,
        "<br>Happiness: ", dat$Happiness,
        "<br>Liveness: ", dat$Liveness
      )
    )
    
    p <- plot_ly(
      df,
      x = ~x,
      y = ~res,
      type = "scatter",
      mode = "markers",
      marker = list(
        size = 9,
        opacity = input$quad_alpha,
        color = brand_colors$accent,
        line = list(color = "white", width = 1)
      ),
      text = ~label,
      hovertemplate = "%{text}<extra></extra>"
    ) %>%
      layout(
        title = list(text = "Interactive Quadratic Residual Diagnostic"),
        xaxis = list(title = input$quad_focus),
        yaxis = list(title = "Residuals"),
        margin = list(l = 70, r = 30, t = 70, b = 70)
      )
    
    if (isTRUE(input$quad_smooth)) {
      sm <- stats::lowess(df$x, df$res)
      smooth_df <- data.frame(x = sm$x, y = sm$y)
      
      p <- p %>%
        add_lines(
          data = smooth_df,
          x = ~x,
          y = ~y,
          inherit = FALSE,
          line = list(color = brand_colors$primary, width = 3),
          hoverinfo = "skip",
          name = "Smooth"
        )
    }
    
    p
  })
  
  output$sentiment_plot <- renderPlotly({
    emotion_levels <- c("joy", "trust", "fear", "surprise",
                        "sadness", "disgust", "anger", "anticipation")
    
    emotion_colors <- c(
      joy = "#44AF69",
      trust = "#74C69D",
      fear = "#56CFE1",
      surprise = "#8E72D9",
      sadness = "#4D96FF",
      disgust = "#B565A7",
      anger = "#E76F51",
      anticipation = "#F4A261"
    )
    
    # 🔑 HARD-CODED ORDER (by release date)
    song_order <- c(
      "Just You & Me",
      "Soft Spot",
      "Seasick",
      "Sweet Sleep",
      "Downtown",
      "Broken Glass",
      "Shadow",
      "Wasting Time With You",
      "Must Be Nice",
      "Untethered",
      "Love Me"
    )
    
    lyric_tokens <- tidytext::unnest_tokens(
      lyrics_df[, c("song", "lyrics")],
      word,
      lyrics
    )
    
    lyric_tokens <- dplyr::anti_join(
      lyric_tokens,
      tidytext::stop_words,
      by = "word"
    )
    
    emotion_df <- dplyr::inner_join(
      lyric_tokens,
      tidytext::get_sentiments("nrc"),
      by = "word"
    )
    
    emotion_df <- emotion_df[emotion_df$sentiment %in% emotion_levels, ]
    
    emotion_df <- dplyr::count(
      emotion_df,
      song,
      sentiment,
      name = "amount"
    )
    
    emotion_df <- tidyr::complete(
      emotion_df,
      song,
      sentiment = emotion_levels,
      fill = list(amount = 0)
    )
    
    # 🔑 APPLY FIXED ORDER
    emotion_df$song_index <- as.numeric(factor(emotion_df$song, levels = song_order))
    emotion_df <- emotion_df[order(emotion_df$song_index), ]
    
    p <- plotly::plot_ly()
    
    for (emo in emotion_levels) {
      d <- emotion_df[emotion_df$sentiment == emo, ]
      d <- d[order(d$song_index), ]
      
      p <- plotly::add_trace(
        p,
        data = d,
        x = ~song_index,
        y = ~amount,
        type = "scatter",
        mode = "lines",
        stackgroup = "one",
        line = list(
          shape = "spline",
          color = emotion_colors[[emo]],
          width = 1
        ),
        fillcolor = emotion_colors[[emo]],
        opacity = 0.85,
        name = emo,
        text = ~paste0(
          "Song: ", song,
          "<br>Emotion: ", sentiment,
          "<br>Amount: ", amount
        ),
        hoverinfo = "text"
      )
    }
    
    plotly::layout(
      p,
      title = "Lyric Emotion Streamgraph (Release Order)",
      xaxis = list(
        title = "Song",
        tickmode = "array",
        tickvals = seq_along(song_order),
        ticktext = song_order
      ),
      yaxis = list(title = "Amount"),
      legend = list(title = list(text = "Emotion")),
      hovermode = "x unified",
      margin = list(l = 60, r = 40, t = 70, b = 120)
    )
  })
  
  va_data <- reactive({
    w <- spotify()
    
    w_titles <- if ("Title" %in% names(w)) {
      w$Title
    } else if ("Song" %in% names(w)) {
      w$Song
    } else if ("song" %in% names(w)) {
      w$song
    } else {
      paste("Song", seq_len(nrow(w)))
    }
    
    wendlo_va <- data.frame(
      Title = w_titles,
      Artist = "Wendlo",
      Pleasantness = as.numeric(w$Happiness),
      Arousal = as.numeric(w$Energy),
      stringsAsFactors = FALSE
    )
    
    wendlo_va <- wendlo_va[complete.cases(wendlo_va[, c("Title", "Artist", "Pleasantness", "Arousal")]), ]
    
    may <- may_data()
    may_va <- data.frame(
      Title = may$Title,
      Artist = may$Artist,
      Pleasantness = may$Happiness,
      Arousal = may$Energy,
      stringsAsFactors = FALSE
    )
    
    may_va <- may_va[complete.cases(may_va[, c("Title", "Artist", "Pleasantness", "Arousal")]), ]
    
    bind_rows(wendlo_va, may_va)
  })
  
  output$va_plot <- renderPlotly({
    df <- va_data()
    
    if (!identical(input$artist_filter, "All")) {
      df <- df[df$Artist == input$artist_filter, ]
    }
    
    df <- df[df$Pleasantness >= input$pleasantness_filter[1] &
               df$Pleasantness <= input$pleasantness_filter[2], ]
    df <- df[df$Arousal >= input$arousal_filter[1] &
               df$Arousal <= input$arousal_filter[2], ]
    
    p <- ggplot(
      df,
      aes(
        x = Pleasantness,
        y = Arousal,
        color = Artist,
        text = paste0(
          "Song: ", Title,
          "<br>Artist: ", Artist,
          "<br>Pleasantness: ", Pleasantness,
          "<br>Arousal: ", Arousal
        )
      )
    ) +
      annotate("rect", xmin = 0, xmax = 50, ymin = 50, ymax = 100,
               fill = brand_colors$soft_blue, alpha = 0.75) +
      annotate("rect", xmin = 50, xmax = 100, ymin = 50, ymax = 100,
               fill = brand_colors$soft_orange, alpha = 0.75) +
      annotate("rect", xmin = 0, xmax = 50, ymin = 0, ymax = 50,
               fill = brand_colors$soft_green, alpha = 0.65) +
      annotate("rect", xmin = 50, xmax = 100, ymin = 0, ymax = 50,
               fill = brand_colors$soft_purple, alpha = 0.65) +
      geom_point(size = 3, alpha = 0.92) +
      geom_vline(xintercept = 50, linetype = "dashed", color = "gray40") +
      geom_hline(yintercept = 50, linetype = "dashed", color = "gray40") +
      annotate("text", x = 25, y = 97, label = "Unpleasant", fontface = "bold", size = 5) +
      annotate("text", x = 75, y = 97, label = "Pleasant", fontface = "bold", size = 5) +
      annotate("text", x = 25, y = 3, label = "Unpleasant", fontface = "bold", size = 5) +
      annotate("text", x = 75, y = 3, label = "Pleasant", fontface = "bold", size = 5) +
      annotate("text", x = 3, y = 75, label = "High Arousal", angle = 90, fontface = "bold", size = 4.5) +
      annotate("text", x = 3, y = 25, label = "Low Arousal", angle = 90, fontface = "bold", size = 4.5) +
      annotate("text", x = 97, y = 75, label = "High Arousal", angle = 270, fontface = "bold", size = 4.5) +
      annotate("text", x = 97, y = 25, label = "Low Arousal", angle = 270, fontface = "bold", size = 4.5) +
      coord_cartesian(xlim = c(0, 100), ylim = c(0, 100), clip = "off") +
      scale_x_continuous(breaks = seq(0, 100, 25), expand = expansion(mult = c(0.02, 0.08))) +
      scale_y_continuous(breaks = seq(0, 100, 25), expand = expansion(mult = c(0.02, 0.08))) +
      scale_color_manual(values = c(
        "Wendlo" = brand_colors$primary,
        "May Erlewine" = brand_colors$secondary,
        "Jackie Evans" = brand_colors$accent
      )) +
      labs(
        title = "Pleasantness-Arousal Map by Artist",
        x = "Pleasantness (Happiness)",
        y = "Arousal (Energy)",
        color = "Artist"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.margin = margin(35, 90, 35, 90),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right",
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 11),
        plot.title = element_text(size = 18, face = "bold", margin = margin(b = 15))
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  emotion_pair_results <- reactive({
    emotion_levels <- c(
      "joy", "trust", "fear", "surprise",
      "sadness", "disgust", "anger", "anticipation"
    )
    
    lyric_tokens <- tidytext::unnest_tokens(
      lyrics_df[, c("song", "lyrics", "streams")],
      word,
      lyrics
    )
    
    lyric_tokens <- dplyr::anti_join(
      lyric_tokens,
      tidytext::stop_words,
      by = "word"
    )
    
    sentiment_join <- dplyr::inner_join(
      lyric_tokens,
      tidytext::get_sentiments("nrc"),
      by = "word"
    )
    
    sentiment_join <- sentiment_join[sentiment_join$sentiment %in% emotion_levels, ]
    
    emotion_counts <- dplyr::count(
      sentiment_join,
      song,
      sentiment,
      name = "amount"
    )
    
    emotion_counts <- tidyr::complete(
      emotion_counts,
      song,
      sentiment = emotion_levels,
      fill = list(amount = 0)
    )
    
    emotion_matrix <- xtabs(amount ~ song + sentiment, data = emotion_counts)
    emotion_wide <- as.data.frame.matrix(emotion_matrix)
    emotion_wide$song <- rownames(emotion_wide)
    rownames(emotion_wide) <- NULL
    
    for (emo in emotion_levels) {
      if (!(emo %in% names(emotion_wide))) {
        emotion_wide[[emo]] <- 0
      }
    }
    
    wide_df <- merge(
      lyrics_df[, c("song", "streams")],
      emotion_wide,
      by = "song",
      all.x = TRUE
    )
    
    wide_df$streams_log <- log1p(as.numeric(wide_df$streams))
    
    pair_list <- combn(emotion_levels, 2, simplify = FALSE)
    
    results <- data.frame(
      Emotion1 = character(length(pair_list)),
      Emotion2 = character(length(pair_list)),
      Pair = character(length(pair_list)),
      Adj_R2 = numeric(length(pair_list)),
      Interaction_p = numeric(length(pair_list)),
      AIC = numeric(length(pair_list)),
      stringsAsFactors = FALSE
    )
    
    for (i in seq_along(pair_list)) {
      pair <- pair_list[[i]]
      emo1 <- pair[1]
      emo2 <- pair[2]
      
      results$Emotion1[i] <- emo1
      results$Emotion2[i] <- emo2
      results$Pair[i] <- paste(emo1, emo2, sep = " + ")
      results$Adj_R2[i] <- NA_real_
      results$Interaction_p[i] <- NA_real_
      results$AIC[i] <- NA_real_
      
      pair_df <- wide_df[, c("song", "streams_log", emo1, emo2)]
      names(pair_df) <- c("song", "streams_log", "x1", "x2")
      pair_df <- pair_df[complete.cases(pair_df), ]
      
      if (nrow(pair_df) >= 4 && length(unique(pair_df$x1)) >= 2 && length(unique(pair_df$x2)) >= 2) {
        pair_df$x1_c <- as.numeric(scale(pair_df$x1, center = TRUE, scale = FALSE))
        pair_df$x2_c <- as.numeric(scale(pair_df$x2, center = TRUE, scale = FALSE))
        
        fit <- lm(streams_log ~ x1_c * x2_c, data = pair_df)
        a3 <- car::Anova(fit, type = 3)
        
        interaction_p <- NA_real_
        row_names <- rownames(a3)
        if ("x1_c:x2_c" %in% row_names) {
          interaction_p <- a3["x1_c:x2_c", "Pr(>F)"]
        }
        
        results$Adj_R2[i] <- summary(fit)$adj.r.squared
        results$Interaction_p[i] <- interaction_p
        results$AIC[i] <- AIC(fit)
      }
    }
    
    results <- results[order(-results$Adj_R2, results$Interaction_p, results$AIC), ]
    rownames(results) <- NULL
    results
  })
  
  output$emotion_pair_table <- renderTable({
    emotion_pair_results()
  }, rownames = FALSE)
  
  output$emotion_pair_plot <- renderPlotly({
    res <- emotion_pair_results()
    req(nrow(res) > 0)
    
    plot_df <- res
    plot_df$Pair <- factor(plot_df$Pair, levels = rev(plot_df$Pair))
    
    bar_colors <- ifelse(
      is.na(plot_df$Adj_R2),
      "#D9D9D9",
      ifelse(plot_df$Adj_R2 >= 0, brand_colors$accent, brand_colors$primary)
    )
    
    y_min <- min(plot_df$Adj_R2, na.rm = TRUE)
    y_max <- max(plot_df$Adj_R2, na.rm = TRUE)
    if (is.infinite(y_min) || is.infinite(y_max)) {
      y_min <- -0.1
      y_max <- 0.1
    }
    
    y_pad <- (y_max - y_min) * 0.12
    if (is.na(y_pad) || y_pad == 0) {
      y_pad <- 0.05
    }
    
    p <- plotly::plot_ly(
      plot_df,
      x = ~Pair,
      y = ~Adj_R2,
      type = "bar",
      marker = list(
        color = bar_colors,
        line = list(color = "white", width = 1)
      ),
      hovertemplate = paste0(
        "Pair: %{x}",
        "<br>Adjusted R²: %{y:.4f}",
        "<extra></extra>"
      )
    )
    
    plotly::layout(
      p,
      title = list(text = "All 28 Emotion Pairs by Interaction Model"),
      xaxis = list(
        title = "Emotion Pair",
        tickangle = -35
      ),
      yaxis = list(
        title = "Adjusted R-squared",
        zeroline = TRUE,
        zerolinecolor = "gray50",
        range = c(y_min - y_pad, y_max + y_pad)
      ),
      margin = list(l = 70, r = 30, t = 70, b = 140)
    )
  })
  
  output$may_anova <- renderTable({
    may <- may_data()
    may <- may[complete.cases(may[, c("BPM", "Happiness", "Liveness", "Popularity")]), ]
    req(nrow(may) > 0)
    
    model_may <- lm(
      Popularity ~ BPM + Happiness + Liveness,
      data = may
    )
    
    a3_may <- car::Anova(model_may, type = 3)
    
    df <- as.data.frame(a3_may)
    df$Predictor <- rownames(df)
    df <- df[, c("Predictor", setdiff(names(df), "Predictor"))]
    rownames(df) <- NULL
    
    df
  }, rownames = FALSE)
  
  may_results_tbl <- reactive({
    m <- lm(
      Popularity ~ BPM + Duration + Danceability + Happiness +
        Acousticness + Speechiness + Loudness,
      data = train_dat()
    )
    
    may <- may_data()
    may <- may[complete.cases(may[, c(
      "BPM", "Duration", "Danceability", "Happiness",
      "Acousticness", "Speechiness", "Loudness", "Popularity"
    )]), ]
    req(nrow(may) > 0)
    
    may$predicted_popularity <- predict(m, newdata = may)
    may$error <- may$Popularity - may$predicted_popularity
    may$abs_error <- abs(may$error)
    
    may
  })
  
  output$may_results <- renderTable({
    may_results_tbl()
  }, rownames = FALSE)
  
  output$may_plot <- renderPlotly({
    may <- may_results_tbl()
    
    p <- plot_ly(
      may,
      x = ~Popularity,
      y = ~predicted_popularity,
      type = "scatter",
      mode = "markers+text",
      text = ~Title,
      textposition = "top center",
      marker = list(
        size = 10,
        color = brand_colors$secondary,
        line = list(color = "white", width = 1)
      ),
      hovertemplate = paste0(
        "Song: %{text}",
        "<br>Actual popularity: %{x}",
        "<br>Predicted popularity: %{y:.1f}",
        "<extra></extra>"
      )
    )
    
    p <- layout(
      p,
      title = list(text = "Actual vs Predicted Popularity"),
      xaxis = list(
        title = "Actual Popularity",
        automargin = TRUE
      ),
      yaxis = list(
        title = "Predicted Popularity",
        automargin = TRUE
      ),
      margin = list(
        l = 90,   # pushes plot right
        r = 40,
        t = 90,   # pushes plot down from title (visually centers)
        b = 70
      )
    )
    
    p
  })
}

shinyApp(ui = ui, server = server)