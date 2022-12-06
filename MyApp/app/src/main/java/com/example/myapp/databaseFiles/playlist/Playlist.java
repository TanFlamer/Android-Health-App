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
    private Integer playlistID; //unique ID for each playlist regardless of user

    private String playlistName; //unique name for each playlist (same name allowed for different user)

    private Integer userID; //user ID of the user who the playlist belongs to

    //constructor for new playlist
    public Playlist(String playlistName, Integer userID) {
        this.playlistName = playlistName;
        this.userID = userID;
    }

    @Ignore //constructor to change playlist name
    public Playlist(Integer playlistID, String playlistName, Integer userID){
        this.playlistID = playlistID;
        this.playlistName = playlistName;
        this.userID = userID;
    }

    //getter for playlist ID
    public Integer getPlaylistID() {
        return playlistID;
    }

    //setter for playlist ID
    public void setPlaylistID(Integer playlistID) {
        this.playlistID = playlistID;
    }

    //getter for playlist name
    public String getPlaylistName() {
        return playlistName;
    }

    //setter for playlist name
    public void setPlaylistName(String playlistName) {
        this.playlistName = playlistName;
    }

    //getter for user ID
    public Integer getUserID() {
        return userID;
    }

    //setter for user ID
    public void setUserID(Integer userID) {
        this.userID = userID;
    }
}
