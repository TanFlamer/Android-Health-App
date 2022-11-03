package com.example.myapp.fragmentsSport.listSport;

public class SportListItem {

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public int getCaloriePerMinute() {
        return caloriePerMinute;
    }

    public void setCaloriePerMinute(int caloriePerMinute) {
        this.caloriePerMinute = caloriePerMinute;
    }

    String type;
    int caloriePerMinute;

    public SportListItem(String type, int caloriePerMinute){
        this.type = type;
        this.caloriePerMinute = caloriePerMinute;
    }
}
