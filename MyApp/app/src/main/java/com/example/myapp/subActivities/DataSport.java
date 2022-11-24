package com.example.myapp.subActivities;

import android.annotation.SuppressLint;
import android.app.DatePickerDialog;
import android.app.TimePickerDialog;
import android.graphics.Color;
import android.os.Bundle;
import android.util.Pair;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.AbsListView;
import android.widget.AdapterView;
import android.widget.Button;
import android.widget.LinearLayout;
import android.widget.ListView;
import android.widget.Spinner;
import android.widget.Toast;

import androidx.appcompat.app.AppCompatActivity;
import androidx.lifecycle.ViewModelProvider;

import com.example.myapp.R;
import com.example.myapp.databaseFiles.entity.Type;
import com.example.myapp.databaseFiles.viewModal.DataSportViewModel;

import java.time.Duration;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;

public class DataSport extends AppCompatActivity {

    DataSportViewModel dataSportViewModel;
    LinearLayout hiddenLayout;
    ListView dataSportList;
    Button buttonDate, buttonAdd, buttonDuration, buttonDelete, buttonEditSave, buttonReturn;
    Spinner typeSpinner;

    int year, month, day;
    int hour, minute;

    HashMap<Pair<Integer, Duration>, Integer> changeLogs;
    SportDataListAdapter sportDataListAdapter;
    TypeSpinnerAdapter spinnerAdapter;
    List<Pair<Pair<Type, Duration>, Boolean>> sportDataList;
    List<Type> typeList;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.data_sport);
        dataSportViewModel = new ViewModelProvider(this).get(DataSportViewModel.class);
        sportDataList = new ArrayList<>();
        typeList = new ArrayList<>();
        Objects.requireNonNull(getSupportActionBar()).setDisplayHomeAsUpEnabled(true);
        initialiseAll();
    }

    public void populateLists(LocalDate date){
        Pair<List<Pair<Type, Duration>>, List<Type>> listPair = dataSportViewModel.populateList(date);

        List<Pair<Type, Duration>> sportsList = listPair.first;
        sportDataList = new ArrayList<>();
        for(Pair<Type, Duration> typeDurationPair : sportsList) sportDataList.add(new Pair<>(typeDurationPair, false));

        typeList = listPair.second;
        resetList();
    }

    public void findAllViewByID(){
        hiddenLayout = findViewById(R.id.hiddenLayout);
        dataSportList = findViewById(R.id.sportDataListView);
        typeSpinner = findViewById(R.id.typeSpinner);
        buttonAdd = findViewById(R.id.buttonAdd);
        buttonDelete = findViewById(R.id.buttonDelete);
        buttonDate = findViewById(R.id.buttonDate);
        buttonDuration = findViewById(R.id.buttonDuration);
        buttonEditSave = findViewById(R.id.buttonEditSave);
        buttonReturn = findViewById(R.id.buttonReturn);
    }

    public void initialiseAll(){
        findAllViewByID();
        initialiseListView();
        initialiseSpinners();
        initialiseButtons();
    }

    public void initialiseListView(){
        dataSportList.setChoiceMode(AbsListView.CHOICE_MODE_MULTIPLE);
        dataSportList.setOnItemClickListener(onItemClickListener);

        sportDataListAdapter = new SportDataListAdapter(this, sportDataList);
        dataSportList.setAdapter(sportDataListAdapter);
    }

    public void initialiseSpinners(){
        typeSpinner.setOnItemSelectedListener(onItemSelectedListener);
        spinnerAdapter = new TypeSpinnerAdapter(this, androidx.appcompat.R.layout.support_simple_spinner_dropdown_item, typeList);
        typeSpinner.setAdapter(spinnerAdapter);
    }

    public void initialiseButtons(){
        initialiseAddButton();
        initialiseDeleteButton();
        initialiseDateButton();
        initialiseDurationButton();
        initialiseBottomButtons();
    }

    public void initialiseAddButton(){
        buttonAdd.setOnClickListener(v -> {
            addSport();
            resetList();
        });
    }

    public void checkAddButton(){
        boolean emptySpinner = typeList.isEmpty();
        boolean validDuration = minute > 0 || hour > 0;
        buttonAdd.setEnabled(!emptySpinner && validDuration);
    }

    @SuppressLint("SetTextI18n")
    public void addSport(){
        Type type = (Type) typeSpinner.getSelectedItem();
        Duration duration = Duration.ofMinutes(hour * 60L + minute);

        sportDataList.add(new Pair<>(new Pair<>(type, duration), false));
        typeList.remove(type);

        buttonDuration.setText("Select Duration");
        hour = minute = 0;

        Pair<Integer, Duration> integerDurationPair = new Pair<>(type.getTypeID(), duration);
        changeLogs.put(integerDurationPair, Objects.requireNonNull(changeLogs.getOrDefault(integerDurationPair, 0)) + 1);
    }

    public void deleteSports(){
        for(int i = sportDataList.size() - 1; i >= 0; i--){
            Pair<Pair<Type, Duration>, Boolean> pairBooleanPair = sportDataList.get(i);
            if(pairBooleanPair.second){
                Pair<Type, Duration> typeDurationPair = pairBooleanPair.first;
                sportDataList.remove(i);
                typeList.add(typeDurationPair.first);
                Pair<Integer, Duration> integerDurationPair = new Pair<>(typeDurationPair.first.getTypeID(), typeDurationPair.second);
                changeLogs.put(integerDurationPair, Objects.requireNonNull(changeLogs.getOrDefault(integerDurationPair, 0)) - 1);
            }
        }
    }

    public void initialiseDeleteButton(){
        buttonDelete.setOnClickListener(v -> {
            deleteSports();
            resetList();
        });
    }

    @SuppressLint("DefaultLocale")
    public void initialiseDurationButton(){
        buttonDuration.setOnClickListener(v -> new TimePickerDialog(DataSport.this, android.R.style.Theme_Holo_Light_Dialog, (timePicker, i, i2) -> {
            hour = i;
            minute = i2;
            buttonDuration.setText(hour == 0 && minute == 0 ? "Select Duration" : String.format("%d:%02d", hour, minute));
            checkAddButton();
        }, hour, minute, true).show());
    }

    @SuppressLint("SetTextI18n")
    public void initialiseBottomButtons(){
        buttonEditSave.setOnClickListener(v -> {
            if(hiddenLayout.getVisibility() == View.GONE){
                buttonEditSave.setText("Save");
                buttonEditSave.setEnabled(false);
                hiddenLayout.setVisibility(View.VISIBLE);
            }
            else {
                if(dataSportViewModel.getSportID() == 0) dataSportViewModel.insertSport(LocalDate.of(year, month, day));
                changeLogs.forEach((pair, operation) -> {
                    int typeID = pair.first;
                    Duration duration = pair.second;
                    if(operation > 0)
                        dataSportViewModel.insertTypeSport(typeID, duration);
                    else if(operation < 0)
                        dataSportViewModel.deleteTypeSport(typeID, duration);
                    else
                        dataSportViewModel.updateTypeSport(typeID, duration);
                });
                finish();
            }
        });
        buttonReturn.setOnClickListener(v -> finish());
    }

    @SuppressLint("DefaultLocale")
    public void initialiseDateButton(){
        Calendar currentDate = Calendar.getInstance();
        year = currentDate.get(Calendar.YEAR);
        month = currentDate.get(Calendar.MONTH);
        day = currentDate.get(Calendar.DAY_OF_MONTH);
        populateLists(LocalDate.of(year, month, day));
        buttonDate.setText(String.format("%02d/%02d/%04d", day, month, year));

        buttonDate.setOnClickListener(view -> new DatePickerDialog(this, (datePicker, i, i1, i2) -> {
            year = i;
            month = i1;
            day = i2;
            populateLists(LocalDate.of(year, month, day));
            buttonDate.setText(String.format("%02d/%02d/%04d", day, month, year));
        }, year, month, day).show());
    }

    public void resetList(){
        resetAdapters();
        resetButtons();
    }

    public void resetAdapters(){
        sportDataListAdapter.notifyDataSetChanged();
        spinnerAdapter.notifyDataSetChanged();
    }

    public void resetButtons(){
        buttonEditSave.setEnabled(!(sportDataList.isEmpty() || changeLogs.isEmpty()) || hiddenLayout.getVisibility() == View.GONE);
        buttonDelete.setEnabled(selected());
    }

    public boolean selected(){
        for(Pair<Pair<Type, Duration>, Boolean> pair : sportDataList)
            if(pair.second)
                return true;
        return false;
    }

    AdapterView.OnItemClickListener onItemClickListener = (parent, view, position, id) -> {
        Pair<Pair<Type, Duration>, Boolean> pairBooleanPair = sportDataList.get(position);
        view.setBackgroundColor(pairBooleanPair.second ? Color.WHITE : Color.BLUE);
        sportDataList.set(position, new Pair<>(pairBooleanPair.first, !pairBooleanPair.second));
        Pair<Pair<Type, Duration>, Boolean> typeDurationPair = (Pair<Pair<Type, Duration>, Boolean>) parent.getItemAtPosition(position);
        Toast.makeText(getApplicationContext(), typeDurationPair.first.first.getName() + " clicked", Toast.LENGTH_SHORT).show();
    };

    AdapterView.OnItemSelectedListener onItemSelectedListener = new AdapterView.OnItemSelectedListener() {
        @Override
        public void onItemSelected(AdapterView<?> parent, View view, int position, long id) {
            checkAddButton();
            Toast.makeText(getApplicationContext(), ((Type)parent.getItemAtPosition(position)).getName() + " clicked", Toast.LENGTH_SHORT).show();
        }

        @Override
        public void onNothingSelected(AdapterView<?> parent) {
            checkAddButton();
        }
    };

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        if (item.getItemId() == android.R.id.home) {
            finish();
            return true;
        }

        return super.onOptionsItemSelected(item);
    }

    public boolean onCreateOptionsMenu(Menu menu) {
        return true;
    }
}