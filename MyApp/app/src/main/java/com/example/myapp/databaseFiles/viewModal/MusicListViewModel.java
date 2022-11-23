package com.example.myapp.databaseFiles.viewModal;

import android.app.Application;
import android.media.MediaMetadataRetriever;
import android.net.Uri;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.LiveData;

import com.example.myapp.MainApplication;
import com.example.myapp.databaseFiles.entity.Song;
import com.example.myapp.databaseFiles.entity.User;
import com.example.myapp.databaseFiles.repository.SongRepository;

import java.util.List;

public class MusicListViewModel extends AndroidViewModel {

    private final MediaMetadataRetriever mediaMetadataRetriever;
    private final SongRepository songRepository;
    private LiveData<List<Song>> songList;
    private final String filePath;
    private int userID;

    public MusicListViewModel(@NonNull Application application) {
        super(application);
        songRepository = ((MainApplication) getApplication()).getSongRepository();
        userID = ((MainApplication) getApplication()).getUserID();
        songList = songRepository.getAllSongs(userID);
        mediaMetadataRetriever = new MediaMetadataRetriever();
        filePath = getApplication().getFilesDir() + "/music/" + userID;
    }

    public void processFile(String fileName, Uri uri){
        List<Song> songList = findSong(userID, fileName);
        if(songList.size() == 0)
            songRepository.insert(new Song(fileName, getSongDuration(uri), userID));
        else
            songRepository.update(songList.get(0));
    }

    public void delete(Song song){
        songRepository.delete(song);
    }

    public List<Song> findSong(int userID, String songName){
        return songRepository.findSong(userID, songName);
    }

    public LiveData<List<Song>> getSongList(){
        return songList;
    }

    public int getUserID() {
        return userID;
    }

    public String getFilePath() {
        return filePath;
    }

    public int getSongDuration(Uri uri){
        mediaMetadataRetriever.setDataSource(getApplication(), uri);
        String duration = mediaMetadataRetriever.extractMetadata(MediaMetadataRetriever.METADATA_KEY_DURATION);
        return Integer.parseInt(duration) / 1000;
    }
}
