package com.example.myappv2.subfragments.sleep;

public class SleepListItem {

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

    String date;
    String sleepTime;
    String wakeTime;
    int sleepDuration;

    public SleepListItem(String date, String sleepTime, String wakeTime, int sleepDuration){
        this.date = date;
        this.sleepTime = sleepTime;
        this.wakeTime = wakeTime;
        this.sleepDuration = sleepDuration;
    }
}
