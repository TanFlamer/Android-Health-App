package com.example.myapp.databasefiles.song;

import static androidx.room.ForeignKey.CASCADE;

import androidx.room.Entity;
import androidx.room.ForeignKey;
import androidx.room.Index;
import androidx.room.PrimaryKey;

import com.example.myapp.databasefiles.user.User;

@Entity(tableName = "Songs",
        indices = { @Index("userID"), @Index("songName") },
        foreignKeys = @ForeignKey(entity = User.class, parentColumns = "userID", childColumns = "userID", onDelete = CASCADE))
public class Song {

    @PrimaryKey(autoGenerate = true)
    private Integer songID;

    private String songName;

    private Integer songDuration;

    private Integer userID;

    public Song(String songName, Integer songDuration, Integer userID) {
        this.songName = songName;
        this.songDuration = songDuration;
        this.userID = userID;
    }

    public Integer getSongID() {
        return songID;
    }

    public void setSongID(Integer songID) {
        this.songID = songID;
    }

    public String getSongName() {
        return songName;
    }

    public void setSongName(String songName) {
        this.songName = songName;
    }

    public Integer getSongDuration() {
        return songDuration;
    }

    public void setSongDuration(Integer songDuration) {
        this.songDuration = songDuration;
    }

    public Integer getUserID() {
        return userID;
    }

    public void setUserID(Integer userID) {
        this.userID = userID;
    }
}
