package com.example.myapp.databaseFiles.songcatalogue;

import static androidx.room.ForeignKey.CASCADE;

import androidx.annotation.NonNull;
import androidx.room.Entity;
import androidx.room.ForeignKey;
import androidx.room.Index;

import com.example.myapp.databaseFiles.playlist.Playlist;
import com.example.myapp.databaseFiles.song.Song;
import com.example.myapp.databaseFiles.user.User;

@Entity(tableName = "SongCatalogue",
        primaryKeys = { "playlistID", "songID" },
        indices = @Index("userID"),
        foreignKeys = { @ForeignKey(entity = Playlist.class, parentColumns = "playlistID", childColumns = "playlistID", onDelete = CASCADE),
                        @ForeignKey(entity = Song.class, parentColumns = "songID", childColumns = "songID", onDelete = CASCADE),
                        @ForeignKey(entity = User.class, parentColumns = "userID", childColumns = "userID", onDelete = CASCADE) })
public class SongCatalogue {

    @NonNull
    private final Integer playlistID; //playlist ID of a specific playlist

    @NonNull
    private final Integer songID; //song ID of a specific song

    @NonNull
    private final Integer userID; //user ID of the user who the song catalogue belongs to

    //constructor for new song catalogue
    public SongCatalogue(@NonNull Integer playlistID, @NonNull Integer songID, @NonNull Integer userID){
        this.playlistID = playlistID;
        this.songID = songID;
        this.userID = userID;
    }

    @NonNull //getter for playlist ID
    public Integer getPlaylistID() {
        return playlistID;
    }

    @NonNull //getter for song ID
    public Integer getSongID() {
        return songID;
    }

    @NonNull //getter for user ID
    public Integer getUserID() {
        return userID;
    }
}
