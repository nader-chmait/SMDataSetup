options("scipen"=100, "digits"=4)

library(dplyr)


lapply_read_csv_bind_rows <- function(path, pattern, skip_rows) {
  files = list.files(path, pattern, full.names = TRUE)
  lapply(files, function(i){
    read.csv(i, header=FALSE, skip=skip_rows)
  }) %>% bind_rows()
}


agg.fb <- lapply_read_csv_bind_rows("/Users/nchmait/Downloads/SS_AO copy/", "Facebook_Post_Engagement.*.csv", 2)
agg.twitter <- lapply_read_csv_bind_rows("/Users/nchmait/Downloads/SS_AO copy/", "Twitter_Post_Engagement.*.csv", 1)
agg.insta <- lapply_read_csv_bind_rows("/Users/nchmait/Downloads/SS_AO copy/", "Instagram_Post_Engagement.*.csv", 1)


colnames(agg.fb) <- c("Post ID", "Shared Content ID", "Message", "Post Date", "Post Time", "Post Type", "Post Visibility", "Img URL", "Social Account", 
                      "Page Slug", "Labels", "Targeting", "Workspace", "Author", "Platform", "Platform URL", "Message Length", "Audit Trail", "External URL", 
                      "Post Stories (Lifetime)-The number of stories generated about your Page post (Stories)", 
                      "Post Stories by Type: Likes (Lifetime)-The number of stories created by a user liking a post",
                      "Post Stories by Type: Shares (Lifetime)-The number of stories created by a user sharing your post", 
                      "Post Stories by Type: Comments (Lifetime)-The number of stories created by a user commenting on your post",
                      "Post Storytellers - Unique Users (Lifetime)-The number of unique people who created a story about your Page post (People Talking About This / PTAT)", 
                      "Likes-The number of unique people who created a story about your Page post by liking the post",
                      "Shares-The number of unique people who created a story about your Page post by sharing your post", 
                      "Comments-The number of unique people who created a story about your Page post by commenting your post",
                      "Interaction Rate (Lifetime)-(Likes + Comments + Link Clicks + Shares) / People Reached (lifetime)", 
                      "Feedback Rate (Lifetime)-(Likes + Comments + Shares) / People Reached (lifetime)", 
                      "Post Consumptions (Lifetime)-The number of times people clicked on anywhere in your posts without generating a story",
                      "Unique Post Consumptions (Lifetime)-The number of people who clicked anywhere in your post without generating a story", 
                      "Post consumptions by Type: link_clicks (Lifetime)-The number of times people clicked on anywhere in your posts without generating a story, by consumption type - link clicks",
                      "Post consumptions by Type: other_clicks (Lifetime)-The number of times people clicked on anywhere in your posts without generating a story, by consumption type - other clicks", 
                      "Post consumptions by Type: photo_view (Lifetime)-The number of times people clicked on anywhere in your posts without generating a story, by consumption type - photo", 
                      "Post consumptions by Type: video_play (Lifetime)-The number of times people clicked on anywhere in your posts without generating a story, by consumption type - video",
                      "Post consumers by type (Lifetime)-The number of people who clicked anywhere in your post, by type. Clicks generating stories are included in Other Clicks. (Unique Users)", 
                      "Unique Post consumptions by Type: link_clicks (Lifetime)-The number of people who clicked anywhere in your post without generating a story, by consumption type - Link Clicks", 
                      "Unique Post consumptions by Type: other_clicks (Lifetime)-The number of people who clicked anywhere in your post without generating a story, by consumption type - Other Clicks", 
                      "Unique Post consumptions by Type: photo_view (Lifetime)-The number of people who clicked anywhere in your post without generating a story, by consumption type - Photo", 
                      "Unique Post consumptions by Type: video_play (Lifetime)-The number of people who clicked anywhere in your post without generating a story, by consumption type - Video", 
                      "Engaged Users - Unique Users - (Lifetime)-The number of people who clicked anywhere in your posts", 
                      "Negative Feedback (Lifetime)-The total number of times people took a negative action in your post (e.g. hide it)", 
                      "Uniques Negative Feedback (Lifetime)-The number of people who took a negative action in your post (e.g. hide it)", 
                      "Negative Feedback by Type: unlike_page_clicks (Lifetime)-The number of times people took a negative action in your post broken down by type - unlike page", 
                      "Negative Feedback by Type: xbutton_clicks (Lifetime)-The number of times people took a negative action in your post broken down by type - xbutton_clicks", 
                      "Negative Feedback by Type: report_spam_clicks (Lifetime)-The number of times people took a negative action in your post broken down by type - report_spam_clicks", 
                      "Uniques Negative Feedback by Type: unlike_page_clicks (Lifetime)-The number of people who took a negative action in your post broken down by type - unlike page", 
                      "Uniques Negative Feedback by Type: xbutton_clicks (Lifetime)-The number of people who took a negative action in your post broken down by type - xbutton_clicks", 
                      "Uniques Negative Feedback by Type: report_spam_clicks (Lifetime)-The number of people who took a negative action in your post broken down by type - report_spam_clicks", 
                      "People Reached (Lifetime)-The number of people who saw your Page post (Unique Users)", 
                      "Organic Reach (Lifetime)-The number of people who saw your post in their Newsfeed or Ticker or on your Page Wall", 
                      "Paid Reach (Lifetime)-The number of people who saw your Page post in an Ad or Sponsored Story", 
                      "Viral Reach (Lifetime)-The number of people who saw your Page post in a story from a friend", 
                      "Fan Reach (Lifetime)-The number of people who have liked your Page who saw your Page post", 
                      "Paid Fan Reach (Lifetime)-The number of people who have like your Page and saw your Page post in an Ad or Sponsored Story", 
                      "Impressions (Lifetime)-The number of impressions for your Page post", 
                      "Organic Impressions (Lifetime)-The number of impressions of your post in Newsfeed, Ticker, or on your Page Wall", 
                      "Paid Impressions (Lifetime)-The number of impressions for your Page post in an Ad or Sponsored Story", 
                      "Viral Impressions (Lifetime)-The number of impressions of your Page post in a story generated by a friend", 
                      "Fan Impressions (Lifetime)-The number of impressions for your Page post by people who have liked your Page", 
                      "Paid Fan Impressions (Lifetime)-The number of impressions for your Page post by people who like your Page in an Ad or Sponsored Story", 
                      "Impressions by Story Type: Fan (Lifetime)-The number of times this post was seen via a story published by a friend of the person viewing the post by story type - liking the page", 
                      "Impressions by Story Type: Comment (Lifetime)-The number of times this post was seen via a story published by a friend of the person viewing the post by story type - comment", 
                      "Impressions by Story Type: Link (Lifetime)-The number of times this post was seen via a story published by a friend of the person viewing the post by story type - link", 
                      "Impressions by Story Type: Other (Lifetime)-The number of times this post was seen via a story published by a friend of the person viewing the post by story type - other", 
                      "Reach by Story Type: Fan (Lifetime)-The number of people who saw your page post in a story from a friend, by story type - liking the page", 
                      "Reach by Story Type: Comment (Lifetime)-The number of people who saw your page post in a story from a friend, by story type - comment", 
                      "Reach by Story Type: Link (Lifetime)-The number of people who saw your page post in a story from a friend, by story type - link", 
                      "Reach by Story Type: Other (Lifetime)-The number of people who saw your page post in a story from a friend, by story type - other", 
                      "Video Duration-The duration of your video.", "Avg. Time Spent-The average length of time people spent viewing your video.", 
                      "Video Views (Lifetime)-Total number of times your video was viewed for more than 3 seconds.", 
                      "Video Views Unique (Lifetime)-The number of unique people who viewed your video for more than 3 seconds.", 
                      "Organic Video Views(Lifetime)-The number of times your video was organically viewed for 3 seconds or more.", 
                      "Organic Video Unique Views (Lifetime)-The number of people who viewed at least 3 seconds of your video without any paid promotion.", 
                      "Paid Video Views (Lifetime)-The number of times your video was viewed via paid impression for 3 seconds or more.", 
                      "Paid Video Unique Views (Lifetime)-The number of people who viewed at least 3 seconds of your video via paid impression.", 
                      "Auto-Played Video Views (Lifetime)-The number of times your video started automatically playing and people viewed it for more than 3 seconds.", 
                      "Clicked-to-Play Video Views (Lifetime)-The number of times people clicked to play your video and viewed it more than 3 seconds.", 
                      "Total Organic Views to 95% (Lifetime)-The number of times your video was organically viewed from the beginning to 95% of its length.", 
                      "Organic Unique Views to 95% (Lifetime)-The number of people who viewed your video organically from the beginning to 95% of its length.", 
                      "Paid Views to 95% (Lifetime)-The number of times your video was viewed via paid impression from the beginning to 95% of its length.", 
                      "Paid Unique Views to 95% (Lifetime)-The number of people who viewed your video via paid impression from the beginning to 95% of its length.", 
                      "Total 30-Second Views (Lifetime)-The total number of times your video was viewed for 30 seconds or viewed to the end, whichever came first.", 
                      "Unique 30-Second Views (Lifetime)-The number of unique people who viewed your video for 30 seconds or to the end, whichever came first.", 
                      "Auto-Played 30-Second Views (Lifetime)-The number of times your video started automatically playing and people viewed it for 30 seconds or to the end, whichever came first.", 
                      "Clicked-to-Play 30-Second Views (Lifetime)-The number of times people clicked to play your video and viewed it for 30 seconds or to the end, whichever came first.", 
                      "Organic 30-Second Views (Lifetime)-The number of times your video was viewed for 30 seconds or viewed to the end, whichever came first, without a paid promotion.", 
                      "Paid 30-Second Views (Lifetime)-The number of times your video was viewed for 30 seconds or viewed to the end, whichever came first, after a paid promotion.", 
                      "Post Reactions: Like (Lifetime)-Total like reactions of a post", "Post Reactions: Love (Lifetime)-Total love reactions of a post", 
                      "Post Reactions: Wow (Lifetime)-Total wow reactions of a post", "Post Reactions: Haha (Lifetime)-Total haha reactions of a post", 
                      "Post Reactions: Sorry (Lifetime)-Total sorry reactions of a post", "Post Reactions: Anger (Lifetime)-Total anger reactions of a post")  

colnames(agg.twitter) <-c("Tweet ID", "Shared Content ID", "Message", "Tweet Date", "Tweet Time", "Tweet Type", "Img URL", "Social Account", "Page Slug", "Labels", 
                          "Workspace", "Author", "Platform", "Platform URL", "Message Length", "Audit Trail", "External URL", "Favorites", "Retweets", "Replies", 
                          "Link Clicks", "Engagement", "Impressions", "Video Views")

colnames(agg.insta) <- c("Post ID", "Shared Content ID", "Message", "Date", "Time", "Type", "Img URL", "Social Account", "Page Slug", "Labels", "Workspace", 
                         "Author", "Platform", "Platform URL", "Message Length", "Audit Trail", "External URL", "Likes", "Comments", "Saved", "Engagement", 
                         "Impressions", "Reach", "Video Views", "Carousel Album Engagement", "Carousel Album Impressions", "Carousel Album Reach", 
                         "Carousel Album Saved", "Carousel Album Video Views", "Exits", "Taps Forward", "Taps Back", "Replies")


library(stringi)
#V1 --> Post Id
agg.fb.1 <- agg.fb[!duplicated(agg.fb$`Post ID`) & agg.fb$`Post ID`!=  "No Data Available", ]
agg.twitter.1 <- agg.twitter[!grepl("reply with #", agg.twitter$Message, ignore.case = T) & !grepl("Here's how the Finals of #AusOpen played out on Twitter.", agg.twitter$Message, ignore.case = T) & !duplicated(agg.twitter$`Tweet ID`) & agg.twitter$`Tweet ID`!=  "No Data Available", ]
agg.insta.1 <- agg.insta[!duplicated(agg.insta$`Post ID`) & agg.insta$`Post ID`!=  "No Data Available", ]

# V4 --> Post Date. V5 --> Post Time .
agg.fb.2 <-  agg.fb.1[order(agg.fb.1$`Post Date`, agg.fb.1$`Post Time`, decreasing = T), ]
agg.twitter.2 <-  agg.twitter.1[order(agg.twitter.1$`Tweet Date`, agg.twitter.1$`Tweet Time`, decreasing = T), ]
agg.insta.2 <-  agg.insta.1[order(agg.insta.1$Date, agg.insta.1$Time, decreasing = T), ]

#V3 --> Message . V4 --> Post Date . 
#Twitter V6--> Tweet Type. Twitter V23 ---> Impressions

agg.fb.3 <- agg.fb.2
agg.twitter.3 <- agg.twitter.2
agg.insta.3 <- agg.insta.2

agg.fb.3$duplicate <- duplicated(agg.fb.2[,c("Message", "Post Date", "Post Type")]) | duplicated(agg.fb.2[,c("Message", "Post Date", "Post Type")], fromLast = TRUE) 
agg.twitter.3$duplicate <-duplicated(agg.twitter.2[,c("Message", "Tweet Date", "Tweet Type")]) | duplicated(agg.twitter.2[,c("Message", "Tweet Date", "Tweet Type")], fromLast = TRUE) 
agg.insta.3$duplicate <- duplicated(agg.insta.2[,c("Message", "Date", "Type")]) | duplicated(agg.insta.2[,c("Message", "Date", "Type")], fromLast = TRUE) 



agg.fb.4 <-  agg.fb.3[!(agg.fb.3$duplicate== TRUE & agg.fb.3$`Post Reactions: Like (Lifetime)-Total like reactions of a post` == 0 &
                          agg.fb.3$`Likes-The number of unique people who created a story about your Page post by liking the post` == 0 & 
                          agg.fb.3$`Impressions (Lifetime)-The number of impressions for your Page post` == 0 & 
                          agg.fb.3$`Video Views (Lifetime)-Total number of times your video was viewed for more than 3 seconds.` ==0), ]
agg.twitter.4 <- agg.twitter.3[!(agg.twitter.3$duplicate == TRUE  & agg.twitter.3$Favorites == 0 & agg.twitter.3$Impressions == 0 & agg.twitter.3$`Video Views` == 0), ]
agg.insta.4 <- agg.insta.3[!(agg.insta.3$duplicate == TRUE & agg.insta.3$Likes == 0  & agg.insta.3$Impressions == 0 & agg.insta.3$`Video Views` == 0), ]

#------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------
agg.fb.4$img.url.length <- nchar(agg.fb.4$`Img URL`) #lapply(agg.fb.4$`Img URL`, FUN = nchar)
agg.fb.4[agg.fb.4$img.url.length>1999, "Img URL"] <- "Cannot Store more than 2000 chars of link" 
#------------------------------------------------------------------------------------------------------
agg.twitter.4$img.url.length <- nchar(agg.twitter.4$`Img URL`) 
agg.twitter.4[agg.twitter.4$img.url.length>1999, "Img URL"] <- "Cannot Store more than 2000 chars of link" 
#------------------------------------------------------------------------------------------------------
agg.insta.4$img.url.length <- nchar(agg.insta.4$`Img URL`) 
agg.insta.4[agg.insta.4$img.url.length>1999, "Img URL"] <- "Cannot Store more than 2000 chars of link" 
#------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------

write.csv(agg.fb.4, file="/Volumes/Files/Marketing/Data & Insights/0. Shared/Social/AO/Social Studio Extracts/Aggregates/Facebook_Engagement_Complete.csv", fileEncoding = "UTF-8", row.names=FALSE)
write.csv(agg.twitter.4, "/Volumes/Files/Marketing/Data & Insights/0. Shared/Social/AO/Social Studio Extracts/Aggregates/Twitter_Engagement_Complete.csv", fileEncoding = "UTF-8", row.names=FALSE)
write.csv(agg.insta.4, "/Volumes/Files/Marketing/Data & Insights/0. Shared/Social/AO/Social Studio Extracts/Aggregates/Instagram_Engagement_Complete.csv", fileEncoding = "UTF-8", row.names=FALSE)




