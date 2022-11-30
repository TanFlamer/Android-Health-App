package com.example.myapp.databasefiles.songcatalogue;

import static androidx.room.ForeignKey.CASCADE;

import androidx.annotation.NonNull;
import androidx.room.Entity;
import androidx.room.ForeignKey;
import androidx.room.Index;

import com.example.myapp.databasefiles.user.User;
import com.example.myapp.databasefiles.playlist.Playlist;
import com.example.myapp.databasefiles.song.Song;

@Entity(tableName = "SongCatalogue",
        primaryKeys = { "playlistID", "songID" },
        indices = @Index("userID"),
        foreignKeys = { @ForeignKey(entity = Playlist.class, parentColumns = "playlistID", childColumns = "playlistID", onDelete = CASCADE),
                        @ForeignKey(entity = Song.class, parentColumns = "songID", childColumns = "songID", onDelete = CASCADE),
                        @ForeignKey(entity = User.class, parentColumns = "userID", childColumns = "userID", onDelete = CASCADE) })
public class SongCatalogue {

    @NonNull
    private Integer playlistID;

    @NonNull
    private Integer songID;

    @NonNull
    private Integer userID;

    public SongCatalogue(@NonNull Integer playlistID, @NonNull Integer songID, @NonNull Integer userID){
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
