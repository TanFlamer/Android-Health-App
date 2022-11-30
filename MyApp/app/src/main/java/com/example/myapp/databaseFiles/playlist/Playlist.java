package com.example.myapp.databasefiles.playlist;

import static androidx.room.ForeignKey.CASCADE;

import androidx.room.Entity;
import androidx.room.ForeignKey;
import androidx.room.Ignore;
import androidx.room.Index;
import androidx.room.PrimaryKey;

import com.example.myapp.databasefiles.user.User;

@Entity(tableName = "Playlists",
        indices = { @Index("userID"), @Index("playlistName") },
        foreignKeys = @ForeignKey(entity = User.class, parentColumns = "userID", childColumns = "userID", onDelete = CASCADE))
public class Playlist {

    @PrimaryKey(autoGenerate = true)
    private Integer playlistID;

    private String playlistName;

    private Integer userID;

    public Playlist(String playlistName, Integer userID) {
        this.playlistName = playlistName;
        this.userID = userID;
    }

    @Ignore
    public Playlist(Integer playlistID, String playlistName, Integer userID){
        this.playlistID = playlistID;
        this.playlistName = playlistName;
        this.userID = userID;
    }

    public Integer getPlaylistID() {
        return playlistID;
    }

    public void setPlaylistID(Integer playlistID) {
        this.playlistID = playlistID;
    }

    public String getPlaylistName() {
        return playlistName;
    }

    public void setPlaylistName(String playlistName) {
        this.playlistName = playlistName;
    }

    public Integer getUserID() {
        return userID;
    }

    public void setUserID(Integer userID) {
        this.userID = userID;
    }
}
