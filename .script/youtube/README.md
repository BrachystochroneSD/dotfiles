# Youtube search Shell Script

I was sick of going to youtube with all of its adds and awfuls recommanded videos with horrible clickbait titles. Then I wrote this simple bash script to access youtube search.

Just Run it, a dmenu prompt will pop and you can search anything you want on youtube. No needs to set up an account.


# Channel Options

You can search for an specific channel with the channel options. When you choose a channel, you can store it in your rss file (**you need to change the save_channel function if you don't use rss**)

# Dependencies:
  + Bash 4
  + mpv (0.14.0)
  + youtube-dl
  + dmenu or rofi or alternative (fzf for example)
  + internet connection (of course.)

# Parameters:
  + youtube api-key (more infos on https://developers.google.com/youtube/v3/getting-started)
    For privacy purpose I store my apikeys variables in an external file and source them in the code.
    You just need to set the variable $ytapikey or rename it to whatever you like.
