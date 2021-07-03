cat ~/.bash_history >> ~/.bash_history_incremental_backup
sort -u ~/.bash_history_incremental_backup -o ~/.bash_history_incremental_backup
