package com.example.myapp.databaseFiles.entity;

import androidx.room.ColumnInfo;
import androidx.room.Entity;
import androidx.room.PrimaryKey;

@Entity(tableName = "Songs")
public class Song {
    @PrimaryKey(autoGenerate = true)
    private int songID;

    @ColumnInfo(name = "song-name")
    private String songName;

    @ColumnInfo(name = "song-duration")
    private int songDuration;

    public Song(String songName, int songDuration) {
        this.songName = songName;
        this.songDuration = songDuration;
    }

    public int getSongID() {
        return songID;
    }

    public void setSongID(int songID) {
        this.songID = songID;
    }

    public String getSongName() {
        return songName;
    }

    public void setSongName(String songName) {
        this.songName = songName;
    }

    public int getSongDuration() {
        return songDuration;
    }

    public void setSongDuration(int songDuration) {
        this.songDuration = songDuration;
    }
}
