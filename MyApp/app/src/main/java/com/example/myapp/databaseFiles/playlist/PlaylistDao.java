package com.example.myapp.databasefiles.playlist;

import androidx.lifecycle.LiveData;
import androidx.room.Dao;
import androidx.room.Delete;
import androidx.room.Insert;
import androidx.room.Query;
import androidx.room.Update;

import java.util.List;

@Dao
public interface PlaylistDao {

    @Insert
    long insert(Playlist playlist); //insertion operation for new playlist

    @Update
    void update(Playlist playlist); //update operation for existing playlist

    @Delete
    void delete(Playlist playlist); //delete operation for existing playlist

    @Query("SELECT * FROM Playlists WHERE userID=:userID AND playlistName=:playlistName")
    Playlist findPlaylist(int userID, String playlistName); //check if playlist with specific name exists

    @Query("SELECT * FROM Playlists WHERE userID=:userID")
    LiveData<List<Playlist>> getAllPlaylists(int userID); //returns live data of all playlist belonging to a user
}
