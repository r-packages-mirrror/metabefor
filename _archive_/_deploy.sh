### In Plesk, execute with:
### bash _deploy.sh >> _deployment.log 2>&1
### Cran job for executing every hour with logging
### @hourly bash ~/git_clone_hpss/deploy.sh >> ~/git_clone_hpss/deployment.log 2>&1
### https://www.cyberciti.biz/faq/how-do-i-add-jobs-to-cron-under-linux-or-unix-oses/
### To edit the cron file with nano instead of vim:
### export VISUAL=nano; crontab -e

echo ----------
echo Beginning at: $(date)
echo ----------

### Go to directory with cloned git repo
cd ~/deploy_metabefor.opens.science

### Delete old 'public' directory if it exists
#rm -rf public

echo Current directory: $(pwd)
echo Current '$PATH': $PATH
echo Running PkgDown:

### Render the site
#/usr/local/bin/quarto render --to all
/usr/local/bin/R -e "devtools::load_all(); pkgdown::build_site();"

echo Finished PkgDown, starting to delete all contents:

### Delete all contents in public HTML directory
rm -rf ~/metabefor.opens.science/*.*
rm -rf ~/metabefor.opens.science/*
rm -f ~/metabefor.opens.science/.htaccess

echo Finished deleting all contents, copying new contents:

### Copy website
cp -RT public ~/metabefor.opens.science

### Copy .htaccess
#cp -f .htaccess ~/metabefor.opens.science

echo ----------
echo Finishing at: $(date)
echo ----------
