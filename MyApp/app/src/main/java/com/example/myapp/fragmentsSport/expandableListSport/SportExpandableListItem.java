package com.example.myapp.fragmentsSport.expandableListSport;

import java.util.List;

public class SportExpandableListItem {

    public String getDate() {
        return date;
    }

    public void setDate(String date) {
        this.date = date;
    }

    public List<SportExpandableListData> getSportData() {
        return sportExpandableListDataList;
    }

    public void setSportData(List<SportExpandableListData> sportExpandableListData) {
        this.sportExpandableListDataList = sportExpandableListData;
    }

    String date;
    List<SportExpandableListData> sportExpandableListDataList;

    public SportExpandableListItem(String date, List<SportExpandableListData> sportExpandableListDataList){
        this.date = date;
        this.sportExpandableListDataList = sportExpandableListDataList;
    }
}
