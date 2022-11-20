package com.example.myapp.fragmentsSport.expandableListSport;

import com.example.myapp.databaseFiles.entity.Sport;

import java.time.LocalDate;
import java.util.List;

public class SportExpandableListItem {

    public LocalDate getDate() {
        return date;
    }

    public void setDate(LocalDate date) {
        this.date = date;
    }

    public List<SportExpandableListData> getSportData() {
        return sportExpandableListDataList;
    }

    public void setSportData(List<SportExpandableListData> sportExpandableListData) {
        this.sportExpandableListDataList = sportExpandableListData;
    }

    LocalDate date;
    List<SportExpandableListData> sportExpandableListDataList;

    public SportExpandableListItem(Sport sport, List<SportExpandableListData> sportExpandableListDataList){
        this.date = sport.getDate();
        this.sportExpandableListDataList = sportExpandableListDataList;
    }
}
