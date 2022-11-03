package com.example.myapp.fragmentsMusic.listMusic;

public class MusicListItem {

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

    public MusicListItem(String name, int length){
        this.name = name;
        this.length = length;
    }
}
