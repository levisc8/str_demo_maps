# Scheduled job to build Compadre and Padrino world maps monthly

This repo hosts a CRON job to build hi-res maps of Padrino and Compadre studies
on a monthly basis. Results are stored `/figures`. Because Padrino doesn't really
exist in the wild yet, the currently digitized studies are stored in a csv in
the `padrino/` folder. It contains coordinates, species, and kingdom information.

# Details

This currently excludes all studies which do not have kingdom, lat, or long information
in the database. Thus, a considerable number of species may be omitted at any given
build. Checks for this number are not run.

The job is scheduled to run at midnight on the first of every month. 

Mostly just working out how these CRON jobs work so I can use them elsewhere.
