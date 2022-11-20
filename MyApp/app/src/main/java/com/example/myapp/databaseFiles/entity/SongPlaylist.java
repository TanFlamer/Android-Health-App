package com.example.myapp.databaseFiles.entity;

import static androidx.room.ForeignKey.CASCADE;

import androidx.annotation.NonNull;
import androidx.room.Entity;
import androidx.room.ForeignKey;
import androidx.room.Index;

@Entity(tableName = "SongPlaylist",
        primaryKeys = { "playlistID", "songID" },
        indices = { @Index("playlistID"), @Index("songID"), @Index("userID") },
        foreignKeys = { @ForeignKey(entity = Playlist.class, parentColumns = "playlistID", childColumns = "playlistID", onDelete = CASCADE),
                        @ForeignKey(entity = Song.class, parentColumns = "songID", childColumns = "songID", onDelete = CASCADE),
                        @ForeignKey(entity = User.class, parentColumns = "userID", childColumns = "userID", onDelete = CASCADE) })
public class SongPlaylist {

    @NonNull
    private Integer playlistID;

    @NonNull
    private Integer songID;

    @NonNull
    private Integer userID;

    public SongPlaylist(@NonNull Integer playlistID, @NonNull Integer songID, @NonNull Integer userID){
        this.playlistID = playlistID;
        this.songID = songID;
        this.userID = userID;
    }

    @NonNull
    public Integer getPlaylistID() {
        return playlistID;
    }

    public void setPlaylistID(@NonNull Integer playlistID) {
        this.playlistID = playlistID;
    }

    @NonNull
    public Integer getSongID() {
        return songID;
    }

    public void setSongID(@NonNull Integer songID) {
        this.songID = songID;
    }

    @NonNull
    public Integer getUserID() {
        return userID;
    }

    public void setUserID(@NonNull Integer userID) {
        this.userID = userID;
    }
}
