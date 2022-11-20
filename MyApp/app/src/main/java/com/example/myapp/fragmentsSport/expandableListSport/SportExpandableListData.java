package com.example.myapp.fragmentsSport.expandableListSport;

import com.example.myapp.databaseFiles.entity.Type;

import java.time.Duration;

public class SportExpandableListData {

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Duration getDuration() {
        return duration;
    }

    public void setDuration(Duration duration) {
        this.duration = duration;
    }

    public double getCalories() {
        return calories;
    }

    public void setCalories(double calories) {
        this.calories = calories;
    }

    String name;
    Duration duration;
    double calories;

    public SportExpandableListData(Type type, Duration duration){
        this.name = type.getName();
        this.duration = duration;
        this.calories = type.getCaloriePerMinute() * duration.toMinutes();
    }
}
