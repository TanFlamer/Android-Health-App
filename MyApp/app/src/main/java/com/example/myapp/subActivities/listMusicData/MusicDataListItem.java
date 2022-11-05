package com.example.myapp.subActivities.listMusicData;

public class MusicDataListItem {

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public int getLength() {
        return length;
    }

    public void setLength(int length) {
        this.length = length;
    }

    String name;
    int length;

    public MusicDataListItem(String name, int length){
        this.name = name;
        this.length = length;
    }
}
