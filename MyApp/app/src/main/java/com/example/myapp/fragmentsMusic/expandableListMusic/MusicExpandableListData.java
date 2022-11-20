package com.example.myapp.fragmentsMusic.expandableListMusic;

import com.example.myapp.databaseFiles.entity.Song;

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

    public MusicExpandableListData(Song song){
        this.name = song.getSongName();
        this.length = song.getSongDuration();
    }
}
