package com.example.myapp.subActivities.sport;

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
import com.example.myapp.databaseFiles.type.Type;
import com.example.myapp.subActivities.type.TypeSpinnerAdapter;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;
import java.util.TimeZone;

public class DataSport extends AppCompatActivity {

    DataSportViewModel dataSportViewModel;
    LinearLayout hiddenLayout;
    ListView dataSportList;
    Button buttonDate, buttonAdd, buttonDuration, buttonDelete, buttonEditSave, buttonReturn;
    Spinner typeSpinner;

    SportDataListAdapter sportDataListAdapter;
    TypeSpinnerAdapter spinnerAdapter;

    HashMap<Pair<Integer, Integer>, Integer> changeLogs;
    List<Pair<Pair<Type, Integer>, Boolean>> sportDataList;
    List<Type> typeList;

    int year, month, day;
    int hour, minute;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.data_sport);
        dataSportViewModel = new ViewModelProvider(this).get(DataSportViewModel.class);
        changeLogs = new HashMap<>();
        sportDataList = new ArrayList<>();
        typeList = new ArrayList<>();
        Objects.requireNonNull(getSupportActionBar()).setDisplayHomeAsUpEnabled(true);
        initialiseAll();
    }

    public void populateLists(long date){
        Pair<List<Pair<Type, Integer>>, List<Type>> listPair = dataSportViewModel.populateList(date);

        sportDataList.clear();
        typeList.clear();

        for(Pair<Type, Integer> typeDurationPair : listPair.first) sportDataList.add(new Pair<>(typeDurationPair, false));
        typeList.addAll(listPair.second);
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
        spinnerAdapter = new TypeSpinnerAdapter(this, android.R.layout.simple_spinner_dropdown_item, typeList);
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
        int duration = hour * 60 + minute;

        sportDataList.add(new Pair<>(new Pair<>(type, duration), false));
        typeList.remove(type);

        buttonDuration.setText("Select Duration");
        hour = minute = 0;

        Pair<Integer, Integer> integerDurationPair = new Pair<>(type.getTypeID(), duration);
        changeLogs.put(integerDurationPair, Objects.requireNonNull(changeLogs.getOrDefault(integerDurationPair, 0)) + 1);
    }

    public void deleteSports(){
        for(int i = sportDataList.size() - 1; i >= 0; i--){
            Pair<Pair<Type, Integer>, Boolean> pairBooleanPair = sportDataList.get(i);
            if(pairBooleanPair.second){
                Pair<Type, Integer> typeDurationPair = pairBooleanPair.first;
                sportDataList.remove(i);
                typeList.add(typeDurationPair.first);
                Pair<Integer, Integer> integerDurationPair = new Pair<>(typeDurationPair.first.getTypeID(), typeDurationPair.second);
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
                long date = LocalDate.of(year, month, day).atStartOfDay(TimeZone.getDefault().toZoneId()).toInstant().toEpochMilli();
                if(dataSportViewModel.getSport() == null) dataSportViewModel.insertSport(date);
                changeLogs.forEach((pair, operation) -> {
                    int typeID = pair.first;
                    int duration = pair.second;
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
        populateLists(LocalDate.of(year, month, day).atStartOfDay(TimeZone.getDefault().toZoneId()).toInstant().toEpochMilli());
        buttonDate.setText(String.format("%02d/%02d/%04d", day, month + 1, year));

        buttonDate.setOnClickListener(view -> new DatePickerDialog(this, (datePicker, i, i1, i2) -> {
            year = i;
            month = i1;
            day = i2;
            populateLists(LocalDate.of(year, month, day).atStartOfDay(TimeZone.getDefault().toZoneId()).toInstant().toEpochMilli());
            buttonDate.setText(String.format("%02d/%02d/%04d", day, month + 1, year));
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
        checkAddButton();
    }

    public boolean selected(){
        for(Pair<Pair<Type, Integer>, Boolean> pair : sportDataList)
            if(pair.second)
                return true;
        return false;
    }

    AdapterView.OnItemClickListener onItemClickListener = (parent, view, position, id) -> {
        Pair<Pair<Type, Integer>, Boolean> pairBooleanPair = sportDataList.get(position);
        view.setBackgroundColor(pairBooleanPair.second ? Color.WHITE : Color.BLUE);
        sportDataList.set(position, new Pair<>(pairBooleanPair.first, !pairBooleanPair.second));
        buttonDelete.setEnabled(selected());
        Pair<Pair<Type, Integer>, Boolean> typeDurationPair = (Pair<Pair<Type, Integer>, Boolean>) parent.getItemAtPosition(position);
        Toast.makeText(getApplicationContext(), typeDurationPair.first.first.getTypeName() + " clicked", Toast.LENGTH_SHORT).show();
    };

    AdapterView.OnItemSelectedListener onItemSelectedListener = new AdapterView.OnItemSelectedListener() {
        @Override
        public void onItemSelected(AdapterView<?> parent, View view, int position, long id) {
            checkAddButton();
            Toast.makeText(getApplicationContext(), ((Type)parent.getItemAtPosition(position)).getTypeName() + " clicked", Toast.LENGTH_SHORT).show();
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