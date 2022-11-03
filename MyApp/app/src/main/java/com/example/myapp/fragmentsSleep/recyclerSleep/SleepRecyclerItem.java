package com.example.myapp.fragmentsSleep.recyclerSleep;

public class SleepRecyclerItem {

    public String getDate() {
        return date;
    }

    public void setDate(String date) {
        this.date = date;
    }

    public String getSleepTime() {
        return sleepTime;
    }

    public void setSleepTime(String sleepTime) {
        this.sleepTime = sleepTime;
    }

    public String getWakeTime() {
        return wakeTime;
    }

    public void setWakeTime(String wakeTime) {
        this.wakeTime = wakeTime;
    }

    public int getSleepDuration() {
        return sleepDuration;
    }

    public void setSleepDuration(int sleepDuration) {
        this.sleepDuration = sleepDuration;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public boolean isHidden() {
        return hidden;
    }

    public void setHidden(boolean hidden) {
        this.hidden = hidden;
    }

    String title;
    String date;
    String sleepTime;
    String wakeTime;
    int sleepDuration;
    boolean hidden;

    public SleepRecyclerItem(String title, String date, String sleepTime, String wakeTime, int sleepDuration){
        this.title = title;
        this.date = date;
        this.sleepTime = sleepTime;
        this.wakeTime = wakeTime;
        this.sleepDuration = sleepDuration;
        this.hidden = false;
    }
}
