package com.example.myapp.fragmentsSport.expandableListSport;

public class SportExpandableListData {

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public int getDuration() {
        return duration;
    }

    public void setDuration(int duration) {
        this.duration = duration;
    }

    public int getCalories() {
        return calories;
    }

    public void setCalories(int calories) {
        this.calories = calories;
    }

    String name;
    int duration;
    int calories;

    public SportExpandableListData(String name, int duration, int calories){
        this.name = name;
        this.duration = duration;
        this.calories = calories;
    }
}
