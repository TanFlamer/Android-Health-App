package com.example.myapp.fragmentsMusic.expandableListMusic;

public class MusicExpandableListData {

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

    public MusicExpandableListData(String name, int length){
        this.name = name;
        this.length = length;
    }
}
