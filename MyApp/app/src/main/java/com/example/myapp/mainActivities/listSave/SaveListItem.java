package com.example.myapp.mainActivities.listSave;

public class SaveListItem {

    public String getLog() {
        return log;
    }

    public void setLog(String log) {
        this.log = log;
    }

    public int getTime() {
        return time;
    }

    public void setTime(int time) {
        this.time = time;
    }

    String log;
    int time;

    public SaveListItem(String log, int time){
        this.log = log;
        this.time = time;
    }
}
