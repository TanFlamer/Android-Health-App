package com.example.myapp.subActivities.listSportData;

public class SportDataListItem {

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public int getDuration() {
        return duration;
    }

    public void setDuration(int duration) {
        this.duration = duration;
    }

    public int getCalorie() {
        return calorie;
    }

    public void setCalorie(int calorie) {
        this.calorie = calorie;
    }

    String type;
    int duration;
    int calorie;

    public SportDataListItem(String type, int duration, int calorie){
        this.type = type;
        this.duration = duration;
        this.calorie = calorie;
    }

}
