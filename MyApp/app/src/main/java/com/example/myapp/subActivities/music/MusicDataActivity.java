package com.example.myapp.subActivities.music;

import android.graphics.Color;
import android.os.Bundle;
import android.text.Editable;
import android.text.TextWatcher;
import android.util.Pair;
import android.view.Menu;
import android.view.MenuItem;
import android.widget.AbsListView;
import android.widget.AdapterView;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ImageView;
import android.widget.ListView;

import androidx.appcompat.app.AppCompatActivity;
import androidx.lifecycle.ViewModelProvider;

import com.example.myapp.R;
import com.example.myapp.databasefiles.song.Song;
import com.google.android.material.textfield.TextInputLayout;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;

public class MusicDataActivity extends AppCompatActivity {

    MusicDataViewModel musicDataViewModel;
    ListView songSelected, songUnselected;
    Button saveButton, returnButton;
    TextInputLayout playlistNameInput;
    ImageView addImageView, removeImageView;
    EditText playlistName;

    private final int ADD_TO_PLAYLIST = 1;
    private final int REMOVE_FROM_PLAYLIST = -1;
    HashMap<Integer, Integer> changeLogs;

    MusicDataListAdapter songUnselectedAdapter;
    MusicDataListAdapter songSelectedAdapter;

    List<Pair<Song, Boolean>> unselectedSongList;
    List<Pair<Song, Boolean>> selectedSongList;

    String namePlaylist;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.data_music);
        //get view model
        musicDataViewModel = new ViewModelProvider(this).get(MusicDataViewModel.class);
        //load existing playlist if name given
        namePlaylist = musicDataViewModel.loadPlaylist(getIntent().getStringExtra("playlistName"));
        //record changes to song list
        changeLogs = new HashMap<>();
        //show back button on top
        Objects.requireNonNull(getSupportActionBar()).setDisplayHomeAsUpEnabled(true);
        //initialise selected and unselected song list
        initialiseSongLists();
        //initialise all components
        initialiseAll();
    }

    //initialise selected and unselected song list
    public void initialiseSongLists(){
        //get selected and unselected song list from view model
        Pair<List<Song>, List<Song>> songLists = musicDataViewModel.populateLists();
        unselectedSongList = new ArrayList<>();
        //add unselected song to new list
        for(Song song : songLists.first) unselectedSongList.add(new Pair<>(song, false));
        selectedSongList = new ArrayList<>();
        //add selected song to new list
        for(Song song : songLists.second) selectedSongList.add(new Pair<>(song, false));
    }

    //initialise all components
    public void initialiseAll(){
        //initialise list views to display selected and unselected songs
        initialiseListViews();
        //initialise edit text for user input
        initialiseEditText();
        //initialise add and remove song image views
        initialiseImageView();
        //initialise save and return button
        initialiseButtons();
    }

    //reset list adapter and image views when changes made
    public void resetLists(){
        //reset list adapter for selected and unselected songs
        resetAdapters();
        //reset add and remove song image views
        resetImageViews();
    }

    //reset add and remove song image views
    public void resetImageViews(){
        //reset add song image views
        setImageView(unselectedSongList, addImageView);
        //reset remove song image views
        setImageView(selectedSongList, removeImageView);
    }

    //reset list adapter for selected and unselected songs
    public void resetAdapters(){
        //reset list adapter for unselected songs
        songUnselectedAdapter.notifyDataSetChanged();
        //reset list adapter for selected songs
        songSelectedAdapter.notifyDataSetChanged();
    }

    //check if any songs from list selected
    public boolean selected(List<Pair<Song, Boolean>> songList){
        for(Pair<Song, Boolean> pair : songList)
            if(pair.second) //if at least one song selected, return true
                return true;
        return false; //if no song selected, return false
    }

    //initialise list views to display selected and unselected songs
    public void initialiseListViews(){
        //initialise list views to display unselected songs
        initialiseSongUnselectedView();
        //initialise list views to display selected songs
        initialiseSongSelectedView();
    }

    //initialise list views to display unselected songs
    public void initialiseSongUnselectedView(){
        //get list view ID
        songUnselected = findViewById(R.id.songUnselected);
        //set multi choice mode
        songUnselected.setChoiceMode(AbsListView.CHOICE_MODE_MULTIPLE);
        //set on item click listener
        songUnselected.setOnItemClickListener(onItemClickListener);
        //initialise new list adapter
        songUnselectedAdapter = new MusicDataListAdapter(this, R.layout.data_music_list_item, unselectedSongList);
        //set new adapter for list view
        songUnselected.setAdapter(songUnselectedAdapter);
    }

    //initialise list views to display selected songs
    public void initialiseSongSelectedView(){
        //get list view ID
        songSelected = findViewById(R.id.songSelected);
        //set multi choice mode
        songSelected.setChoiceMode(AbsListView.CHOICE_MODE_MULTIPLE);
        //set on item click listener
        songSelected.setOnItemClickListener(onItemClickListener);
        //initialise new list adapter
        songSelectedAdapter = new MusicDataListAdapter(this, R.layout.data_music_list_item, selectedSongList);
        //set new adapter for list view
        songSelected.setAdapter(songSelectedAdapter);
    }

    //move songs from one list to the other
    public void moveSongs(List<Pair<Song, Boolean>> songListFrom, List<Pair<Song, Boolean>> songListTo, int operation){
        for(int i = songListFrom.size() - 1; i >= 0; i--){ //loop through list to find selected song
            Pair<Song, Boolean> songBooleanPair = songListFrom.get(i);
            if(songBooleanPair.second){ //if song selected
                Song song = songBooleanPair.first;
                songListFrom.remove(i); //remove song from old list
                songListTo.add(new Pair<>(song, false)); //add song to new list
                int finalOperation = Objects.requireNonNull(changeLogs.getOrDefault(song.getSongID(), 0)) + operation;
                if(finalOperation == 0) //if song is back in original list, remove from change logs
                    changeLogs.remove(song.getSongID());
                else //else if song in new list, add to change logs
                    changeLogs.put(song.getSongID(), finalOperation);
            }
        }
    }

    //initialise add and remove song image views
    public void initialiseImageView(){
        //initialise add song image views
        initialiseAddImageView();
        //initialise remove song image views
        initialiseRemoveImageView();
        //reset list adapter and image views
        resetLists();
    }

    //initialise add song image views
    public void initialiseAddImageView(){
        //get image view by ID
        addImageView = findViewById(R.id.addImageView);
        //set on click listener
        addImageView.setOnClickListener(v -> {
            //move songs from unselected list to selected list
            moveSongs(unselectedSongList, selectedSongList, ADD_TO_PLAYLIST);
            //reset list adapter and image views
            resetLists();
            //reset save button
            checkButton();
        });
    }

    //initialise remove song image views
    public void initialiseRemoveImageView(){
        //get image view by ID
        removeImageView = findViewById(R.id.removeImageView);
        //set on click listener
        removeImageView.setOnClickListener(v -> {
            //move songs from selected list to unselected list
            moveSongs(selectedSongList, unselectedSongList, REMOVE_FROM_PLAYLIST);
            //reset list adapter and image views
            resetLists();
            //reset save button
            checkButton();
        });
    }

    //change image view look and list view item colour when clicked
    public boolean updateImageView(List<Pair<Song, Boolean>> songList, int position, ImageView imageView){
        Pair<Song, Boolean> songBooleanPair = songList.get(position);
        //invert selected boolean on clicked song
        songList.set(position, new Pair<>(songBooleanPair.first, !songBooleanPair.second));
        //change image view depending if any song selected
        setImageView(songList, imageView);
        //return new selected state for song
        return !songBooleanPair.second;
    }

    //change image view depending if any song selected
    public void setImageView(List<Pair<Song, Boolean>> songList, ImageView imageView){
        //check if any song selected
        boolean selected = selected(songList);
        //if no song selected, set alpha to 0.35 else 1
        imageView.setAlpha((float)(selected ? 1 : 0.35));
        //if no song selected, set image view to un-clickable else clickable
        imageView.setClickable(selected);
    }

    //initialise edit text for user input
    public void initialiseEditText(){
        //link components by ID
        playlistNameInput = findViewById(R.id.playlistNameInput);
        playlistName = findViewById(R.id.playlistName);
        //set original playlist name if given else empty
        playlistName.setText(namePlaylist);
        //add text watcher
        playlistName.addTextChangedListener(playlistNameTextWatcher);
        //add focus listener
        playlistName.setOnFocusChangeListener((v, hasFocus) -> validatePlaylistName(playlistNameInput, playlistName));
    }

    //initialise save and return button
    public void initialiseButtons(){
        //get save button by ID
        saveButton = findViewById(R.id.editSaveButton);
        //set save button on click listener
        saveButton.setOnClickListener(v -> {
            //get playlist name
            String newPlaylistName = playlistName.getText().toString();
            if(musicDataViewModel.getPlaylist() == null) //if new playlist, insert to database
                musicDataViewModel.insertPlaylist(newPlaylistName);
            else if(!namePlaylist.equals(newPlaylistName)) //if existing playlist and different name, update in database
                musicDataViewModel.updatePlaylist(newPlaylistName);
            changeLogs.forEach((songID, operation) -> { //loop through changes logs
                if(operation > 0) //if new song in selected list
                    musicDataViewModel.insertSongPlaylist(songID); //insert new song catalogue
                else if(operation < 0) //if new song in unselected list
                    musicDataViewModel.deleteSongPlaylist(songID); //delete old song catalogue
            });
            finish(); //return to last activity
        });
        //get return button by ID
        returnButton = findViewById(R.id.returnButton);
        //set return button on click listener to return to last activity
        returnButton.setOnClickListener(v -> finish());
    }

    //validate playlist name
    public boolean validatePlaylistName(TextInputLayout textInputLayout, EditText editText){
        //get playlist name entered
        String playlistText = editText.getText().toString();
        //check if text view has focus
        boolean hasFocus = editText.hasFocus();
        //check if playlist name empty
        boolean emptyPlaylistName = playlistText.isEmpty();
        //check if playlist name taken
        boolean validPlaylistName = !emptyPlaylistName && (playlistText.equals(namePlaylist) || musicDataViewModel.validatePlaylistName(playlistText));

        if(!hasFocus || validPlaylistName) //if no focus or valid name
            textInputLayout.setErrorEnabled(false); //show no error
        else if(emptyPlaylistName) //if empty name
            textInputLayout.setError("Playlist name cannot be empty"); //show error
        else //if invalid name
            textInputLayout.setError("Playlist name already taken"); //show error
        return validPlaylistName; //return validity of playlist name
    }

    //check if playlist name different
    public boolean differentPlaylistName(EditText editText){
        //get playlist name entered
        String playlistText = editText.getText().toString();
        //check if playlist name empty
        boolean emptyPlaylistName = playlistText.isEmpty();
        //return playlist name difference status
        return !emptyPlaylistName && !playlistText.equals(namePlaylist);
    }

    //enable save button if changes detected and valid playlist name
    public void checkButton(){
        //check if playlist name different
        boolean differentPlaylistName = differentPlaylistName(playlistName);
        //check if playlist name valid
        boolean validPlaylistName = validatePlaylistName(playlistNameInput, playlistName);
        //check if selected playlist not empty
        boolean emptyPlaylist = selectedSongList.isEmpty();
        //check if no changes to playlist
        boolean noChanges = changeLogs.isEmpty();
        //set save button if changes detected and valid playlist name
        saveButton.setEnabled(!emptyPlaylist && (differentPlaylistName || (!noChanges && validPlaylistName)));
    }

    //text watcher for playlist name
    private final TextWatcher playlistNameTextWatcher = new TextWatcher() {
        @Override
        public void beforeTextChanged(CharSequence s, int start, int count, int after) {

        }

        @Override
        public void onTextChanged(CharSequence s, int start, int before, int count) {
            //enable save button if changes detected and valid playlist name
            checkButton();
        }

        @Override
        public void afterTextChanged(Editable s) {

        }
    };

    //on item click listener for list view
    public AdapterView.OnItemClickListener onItemClickListener = (parent, view, position, id) -> {
        //get selected state of song
        boolean selected = parent.equals(songUnselected) ? updateImageView(unselectedSongList, position, addImageView) : updateImageView(selectedSongList, position, removeImageView);
        //change song view colour depending on selected state
        view.setBackgroundColor(selected ? Color.LTGRAY : Color.WHITE);
    };

    @Override //set back button on top to return to last activity
    public boolean onOptionsItemSelected(MenuItem item) {
        if (item.getItemId() == android.R.id.home) {
            finish();
            return true;
        }

        return super.onOptionsItemSelected(item);
    }

    //initialise options menu
    public boolean onCreateOptionsMenu(Menu menu) {
        return true;
    }
}