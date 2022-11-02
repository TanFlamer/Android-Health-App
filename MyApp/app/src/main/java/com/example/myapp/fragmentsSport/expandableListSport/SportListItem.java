package com.example.myapp.fragmentsSport.expandableListSport;

import com.example.myapp.fragmentsSport.expandableListSport.SportData;

import java.util.List;

public class SportListItem {

    public String getDate() {
        return date;
    }

    public void setDate(String date) {
        this.date = date;
    }

    public List<SportData> getSportData() {
        return sportData;
    }

    public void setSportData(List<SportData> sportData) {
        this.sportData = sportData;
    }

    String date;
    List<SportData> sportData;

    public SportListItem(String date, List<SportData> sportData){
        this.date = date;
        this.sportData = sportData;
    }
}
