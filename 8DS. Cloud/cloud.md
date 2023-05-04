# Google Cloud & Linux

## Google Cloud

---

### Why Google Cloud?

* There is no particular reason, all cloud providers deliver similar products for a similar cost
* Other major cloud computing providers: Amazon Web Services, Microsoft Azure, Oracle Cloud, IBM Cloud, Alibaba Cloud... My personal favourite: DigitalOcean
* Google Cloud has a nice free tier and $300 credits to use

![](https://i.imgur.com/NzG9nsC.png)

---

### Why learn cloud computing?

* companies want it :shrug: 
* pay-as-you-use pricing
* scalability: you may need little computing power for one project and a lot for another
* security and robustness: the provider manages internet, hardware, backups, security etc. (although it is still a bad idea to post an API or SSH key in a public GitHub repository)
* development speed: cloud companies provide tools to make developers' life easier

---

### Registration

* hopefully you did what you were asked to do: signing up at https://cloud.google.com and verifying your card
* if not, please go to https://console.cloud.google.com/compute/instances, create a project, sign up and verify your phone number & card details (do not worry, if you activate the trial if will be free; if not, the cost will be minimal)
* a few złotys which are charged to verify the card are quickly returned (after a few hours)
* activate the Free Trial, which will give you $300 to use (upper part of the website, Activate) for 90 days

---

## Create instance

https://console.cloud.google.com/compute/instancesAdd

Default region & zone should be fine

machine type: e2-micro

Boot disk: please change to Operating system -- Ubuntu, Version -- Ubuntu 22.04 LTS (x86/64)

Allow HTTP/HTTPS access

---

### Instance

The instance has a name, zone, internal IP and external IP. To connect to the instance, you need to use external IP. On Windows, you may connect through PuTTY, on Linux/Mac you do not need additional software, but the easiest option is to connect through website. Click on SSH https://console.cloud.google.com/compute/instances

After a few seconds, you should see a welcome message with system load, basic diagnostic information, and login info. We are right now in a **Linux console**.

---

### Exercise 1

*note: if you failed to create the instance for whatever reason, you may use https://bellard.org/jslinux/ (Alpine Linux 3.12.0, Console) to do the exercises.*

Please create an instance as stated previously. Type `uname -a` and press [Enter]. Type `touch test.txt` and press [Enter]. Type `ls -l` and press [Enter]. Copy everything you see to a text file and upload it to your GitHub repository. (selecting the text in the console should be enough to copy it)

---

### Switch off the instance

You may stop the instance by selecting it and clicking Stop in the upper menu. A popup with confirmation appears, click Stop once again.

When the instance is not used, you are not charged for running it. *However, you are charged for the disk space used, so keep that in mind*.

You may restart the instance by clicking Start / Resume in the upper menu. The file you created in the exercise should still be visible (`ls -l`).

If you want to delete the instance, you may click the three dots or select the instance and click Delete. It will also remove the disk, so no costs will be incurred. Please do not do this right now, as we will need the instance for this and the next class. (unless you wanted to save the Free trial or the credits for later)

---

## Linux command line

![](https://i.redd.it/u9e6jcgorvr51.png)

---

### Why use Linux?

* Free
* Open source
* As stable, secure, fast as you need
* Even some Microsoft applications are built on Linux as opposed to Windows (Server)
* Used in companies much more often relative to other operating systems -- **get ahead of competition** by using it at home too

---

### Why use command line?

* It is faster
* It forces you to understand what happens in the operating system
* It makes you feel like a hacker 💻
* It helps with troubleshooting
* **It is reproducible** (in contrast to clicking in GUI)

---

### Terminal

white letters on black background
Unix shell: "command-line interpreter or shell that provides a command line user interface for Unix-like operating systems" (from Wikipedia)
Most commonly used shell is *bash* and we will use it, but I personally prefer *fish* (it has revolutionary features like syntax highlighting)

---

### Basic commands

Most commands should be the same on Mac.

[Tab] autocompletes in bash.

---

### Directory navigation

* `cd /dir1/dir2/dir3` to move to given directory
  * `cd ..` to move up (to `/dir1/dir2`), `cd .` to move to current directory, `cd ~` to move to home directory for the current user, `cd /` to move to root directory
* `pwd` to print current path
* `mkdir dirname` to create directory
* `rmdir dirname` to remove directory
* `ls` to print all files, `ls -l` to print all files with permissions/owners/size/modification date, `ls -ltr` to print all files sorted from oldest to newest

---

### Do I have to memorize all the parameters?

Fortunately: no

`ls --help`
`man ls`

In general, all commands should have `--help` (or sometimes `-h`) available, or a manual.

---

### Users

* `sudo` before command to run it as root
* `su _username_` to switch user to \_username\_

---

### File management

* `touch file` to create file (if does not exist) or change modification date (if exists)
* file editors: `vi` or `nano` are the basic options. `nano` is **much** simpler and easier to quit.
* `nano filename` opens `filename` to edit. [Ctrl]+[X] quit, [Ctrl]+[O] save, [Ctrl]+[W] find, [Ctrl]+[K] cut. All necessary commands are at the bottom
* `vi` is more complex, but way more powerful. To quit: [Esc], :q! [Enter]. To write: [Esc], :w [Enter]. You can also concatenate commands.

---

![](https://i.imgur.com/xMMk8hx.png)

---

### File manipulation

* `cp source destination` to copy from *source* to *destination*
* `mv source destination` to move from *source* to *destination*
* `diff file1 file2` to show differences between files
* `rm file` to remove file
* `rm -r directory` to remove directory **and, recursively, all its contents** (use responsibly)
* `chown user:group file` to change file ownership
* `chmod 000 file` to set 000 permissions to file. 1 (execute) + 2 (write) + 4 (read), first number for the owner, second number for the group, third number for others. For example 754: owner may do everything, group may read and execute (but cannot modify), others may only read

---

### Exercise 2

1. Create directory `rr`
2. Switch to the newly created directory
3. Create an empty file `test.txt`
4. Create another file, `new.txt`, using a text editor. Write a few characters there.
5. Copy `/etc/modules` (or any other text file from `/etc`) to the directory
6. Print the contents of the directory in the long listing format, sorted by size from the smallest to the largest file
7. Try to remove directory `rr` using `rmdir` command
8. Actually remove everything from `rr` directory, including the directory

Please copy everything from the console and upload a text file into GitHub.

---

### System

* `df` to print filesystem space usage (`df -h` nicer)
* `du` to print disk usage for file (`du -a . | sort -n -r | head -n 10` shows 10 largest files in directory, google & StackOverflow helps)
* `top` or `htop` to display processes (like Task manager in Windows)
* `ps aux` to print all running processes
* `kill PID` to shut down a process with given process ID number, `kill -9 PID` to stop it immediately
* `ip a` to print network information
* `history` to display history of commands

---

### Pipe

Did you notice the `|` command? Output of command on the left becomes an input of the command on the right. Compare: `%>%` in R.

| du -a .  | sort -n -r | head -n 10 |
| -------- | -------- | -------- |
| disk usage for all files     | sort according to numerical value, reverse     | 10 first lines     |

* `head -n N`, `tail -n N` to print first/last *N* lines
* `cat filename` to print contents of *filename*
* `grep PATTERN` to search for patterns

---

### Automating tasks

`crontab -e`

` m h  dom mon dow   command `: minute, hour, day of month, month, day of week, command. \* means nothing, just ignore and run regardless of unit of time.

For example:
`0 5 * * 1 tar -zcf /var/backups/home.tgz /home/` backup all user accounts @ 5 AM every week
`*/5 * * * * /home/delab/miniconda3/envs/py/bin/python3 /home/delab/pathtofile/download.py` run a Python script every 5 minutes

---

### Bash scripts

Let us write a basic script:

`nano test.sh`
`#!/bin/sh` [Enter]
`echo 'i am a script'` [Ctrl]+[O], [Ctrl]+[X]
`chmod +x test.sh` (make it executable, you may check what changed in `ls -l`)
`./test.sh` (note the dot and slash -- it tells the console to interpret it as a file to execute, not as a command)


---

### Exercise 3

1. Print all processes which contain the string *cpu*.
2. Print the last 3 lines of network information.
3. Switch to any subdirectory of `/etc` which contains at least one (readable) file.
4. Print the directory name.
5. Print the first 3 lines of any file in the directory.

Please copy everything from the console and upload a text file into GitHub. Add the line appended to *crontab* to a separate text file.

---

## Set up a website

    sudo apt update
    sudo apt install nginx-core
    
This should be enough *for now*. Everything is set up, and you may access the website using the External IP from the console (make sure to replace `https` with `http`). But this is only a static site, and we want to do more -- like creating an API, or a dynamic website allowing users to do computations...

---

### Create an app

We will be using Python and Flask. In R's Shiny, things should be similar.

    cd /var/www
    sudo mkdir app
    sudo chown _username_:_username_ app
    
(username should be without underscores)
    
It creates a directory for our application. Keep in mind that we need to change the ownership of the directory, as we create is as root.

    cd app
    nano app.py
    
The contents of `app.py` should be a simple Flask application. We do not need to run it locally.

    import flask

    application = flask.Flask(__name__)

    @application.get('/hello')
    def hello():
        return 'Hello!'

    if __name__ == '__main__':
        application.run()

Flask is not installed, let's fix it.
    
    sudo apt install python3-flask
    
uWSGI allows us to run. `uwsgi.ini` file should look like this:

    [uwsgi]
    socket = /var/www/app/mysite.sock
    chmod-socket = 666
    chdir = /var/www/app
    master = true
    file = app.py
    uid = _username_
    gid = _username_
    mount = /app=app.py
    plugins = python3
    
Naturally, you need to install uWSGI and the Python 3 plugin.

    sudo apt install uwsgi-core uwsgi-plugin-python3
    
Now, we can run the uWSGI application. Go to `screen` (a kind of virtual desktop) and run `uwsgi uwsgi.ini` from the `/var/www/app` directory. Exit using Ctrl+A, Ctrl+D. You can go back to the screen using `screen -r` command.

Now move to `/etc/nginx/sites-enabled` directory and create `app` file (you need root permissions).

    server {
            listen 5000 default_server;
            listen [::]:5000 default_server;

            # root /var/www/app;

            # Add index.php to the list if you are using PHP
            index index.html index.htm index.nginx-debian.html;

            server_name _;

            location / {
                    try_files $uri @uwsgi;
            }

            location @uwsgi {
                include uwsgi_params;
                uwsgi_pass _flask_app;
            }
    }

    upstream _flask_app {
        server unix:///var/www/app/mysite.sock;
    }
    
We are now close to the end, but we still didn't do one thing: allow firewall to connect to the 5000 port.

Go to VM Instances, select three dots and View network details. Click on Firewall -> Create firewall rule. Source IPv4 ranges should be 0.0.0.0/0 (all IPs), TCP port 5000 should be opened.

Now restart nginx (`sudo systemctl restart nginx`), run uWSGI again if necessary (`screen -r`, Ctrl+C, `uwsgi uwsgi.ini`, Ctrl+A, Ctrl+D) and type your External IP:5000/hello into the address bar.

---

## What's next?

* cryptography, public/private key pairs
* security
    *  why not to put everything in /var/www
    *  firewall rules, allow/block traffic
* containers, Docker, Kubernetes
* AppEngine, BigQuery
* continuous integration & continuous deployment
* domains, redirections
